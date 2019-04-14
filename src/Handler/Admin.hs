{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}

module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Sex
import Ageclass
import Weightclass

import qualified Data.Text as T

import Data.CSV.Conduit
import Scoresheetlogic
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Prelude as P

import           Text.RE.Replace
import           Text.RE.TDFA.Text

import Control.Lens (view, set)
import Control.Lens.Setter

latexTemplate :: String
latexTemplate = "latexexport/template.tex"

newtype FileForm = FileForm
    { fileInfo :: FileInfo }

resetDB :: Handler ()
resetDB = do
  runDB $ deleteWhere ([] :: [Filter Lifter])
  runDB $ deleteWhere ([] :: [Filter LifterBackup])
  runDB $ deleteWhere ([] :: [Filter MeetState])

getELiftersFromDB :: Handler [Entity Lifter]
getELiftersFromDB = runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter])

fromEntity :: Entity a -> a
fromEntity (Entity _ x) = x

fromEntities :: [Entity a] -> [a]
fromEntities = map fromEntity

getLiftersFromDB :: Handler [Lifter]
getLiftersFromDB = fromEntities <$> getELiftersFromDB

getCurrMeetStateFromDB :: Handler MeetState
getCurrMeetStateFromDB = do
    dataFromDB <- runDB $ selectList ([] :: [Filter MeetState]) []
    return $
      if ((length dataFromDB) > 0) then
        P.head [ms | (Entity _ ms)<-dataFromDB]
      else
        emptyMeetState

getDataFromDB :: Handler (MeetState, [Lifter])
getDataFromDB =
  (,) <$> getCurrMeetStateFromDB <*> getLiftersFromDB

getLatestBackupVersion :: Handler (Maybe Int)
getLatestBackupVersion =
  do
    versionDB <- runDB $ selectFirst [] [Desc LifterBackupVersion]
    return $ do
      entitym <- versionDB
      let version = lifterBackupVersion $ entityVal entitym
      return version

backupLifter :: [Lifter] -> Int -> Handler ()
backupLifter lifterList v = do
                      let insertList = [LifterBackup v na ag se acl wcl we ra gr res cl | (Lifter na ag se acl wcl we ra gr res cl)<-lifterList] :: [LifterBackup]
                      _ <- runDB $ insertMany insertList
                      return ()

truncBackupHistory :: Handler ()
truncBackupHistory = do
                       backupDB <- runDB $ selectList [] [Desc LifterBackupVersion]
                       let backupVersions = group $ map (lifterBackupVersion . entityVal) backupDB
                       case (length backupVersions) >= 10 of
                         True -> do
                                   let deleteAfterVersion = P.head $ backupVersions P.!! 9
                                   runDB $ deleteWhere [LifterBackupVersion >. deleteAfterVersion]
                         False -> return ()

pushDataToChannel :: (MeetState, [Lifter]) -> Handler ()
pushDataToChannel (ms, lifters) = do
  wChan <- appFrontendChannel <$> getYesod
  atomically $ writeTChan wChan (ms, lifters)

restoreBackup :: Handler ()
restoreBackup = do
                  version <- getLatestBackupVersion
                  case version of
                    Nothing -> return ()
                    Just v -> do
                              backup <- runDB $ selectList [LifterBackupVersion ==. v] []
                              _ <- runDB $ deleteWhere ([] :: [Filter Lifter]) -- truncate table
                              _ <- runDB $ deleteWhere [LifterBackupVersion ==. v]
                              -- restore
                              let insertBack = [Lifter na ag se acl wcl we ra gr res cl | (LifterBackup _ na ag se acl wcl we ra gr res cl) <- map entityVal backup]
                              _ <- runDB $ insertMany insertBack
                              return ()

meetStateForm :: MeetState -> Html -> MForm Handler (FormResult MeetState, Widget)
meetStateForm MeetState {..} = renderDivs $
                               MeetState <$> areq (selectFieldList list) "CurrDiscipline" (Just meetStateCurrDiscipline)
                                         <*> areq intField "GroupNr: " (Just meetStateCurrGroupNr)
  where
    double a = (a,a)
    list :: [(Text,Text)]
    list = map (double . fstMeetType) meetType

getAdminR :: Handler Html
getAdminR = do
    maid <- maybeAuthId
    (formWidget, formEnctype) <- generateFormPost csvForm
    meetState <- getCurrMeetStateFromDB
    eLifters <- getELiftersFromDB
    (lifterformWidget, lifterformEnctype) <- generateFormPost $ liftersForm meetState eLifters
    (meetStateFormWidget, meetStateEnctype) <- generateFormPost $ meetStateForm meetState

    defaultLayout $ do
        setTitle "Welcome to the mighty Scoresheet"
        $(widgetFile "adminpage")

csvForm :: Form FileForm
csvForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Wählen Sie die Anmeldungsdatei aus."

attemptForm :: Attempt -> MForm Handler (FormResult Attempt, Widget)
attemptForm att = do
  (weightRes, weightView) <- mopt doubleField fieldFormat (Just $ attemptWeight att)
  (succRes, succView)     <- mreq (selectFieldList succType) "" (Just $ attemptToModifier att)
  let attRes = createAttempt <$> weightRes <*> succRes
  return (attRes, [whamlet| ^{fvInput weightView} ^{fvInput succView}|])

  where
    fieldFormat = FieldSettings "" Nothing Nothing Nothing [("class", "tableText")]
    succType :: [(Text,LiftModifier)]
    succType = [("Todo", MTodo), ("Good", MGood), ("Fail", MFail), ("Skip", MSkip)]
    createAttempt weight suc =
      case (weight, suc) of
        (Just w, MGood) -> Success w
        (Just w, MFail) -> Fail w
        (Just w, MTodo) -> Todo w
        (_, MSkip)      -> Skip
        (_, _)          -> Unset

disciplineForm :: Text -> Discipline -> MForm Handler (FormResult Discipline, Widget)
disciplineForm descr Discipline { .. } =
  do
    (att1f, att1View) <- attemptForm att1
    (att2f, att2View) <- attemptForm att2
    (att3f, att3View) <- attemptForm att3
    let widget = [whamlet| ^{att1View} ^{att2View} ^{att3View}|]
    let disciplineRes = Discipline <$> att1f <*> att2f <*> att3f
    return (disciplineRes, widget)

resForm :: Results -> MForm Handler (FormResult Results, Widget)
resForm res =
  do
    discForms <- forM meetType (\(n, l) -> disciplineForm n ((view l) res))
    let widgets = fmap snd discForms

    let discWidgets = F.foldl' (>>) [whamlet| |] widgets
    let resChanges =  (zipWith resChangesf (fst <$> discForms) meetType) :: [Maybe Results -> Maybe Results]
    let resRes = F.foldl' (\r f -> f r) (Just $ res) resChanges
    return $ case resRes of
      Just r -> (pure r, discWidgets)
      _      -> (FormFailure ["Error parsing results form"], discWidgets)

    where
      resChangesf :: FormResult Discipline -> (Text, ASetter' Results Discipline) -> Maybe Results -> Maybe Results
      resChangesf (FormSuccess d) (_,l) = fmap $ set l d
      resChangesf _               _     = id

lifterForm :: Entity Lifter -> MForm Handler (FormResult (Entity Lifter), Widget)
lifterForm (Entity lId Lifter {..}) = do
  (idRes,idView) <- mreq hiddenField fieldFormat $ Just lId
  (groupRes, groupView) <- mreq intField fieldFormat $ Just lifterGroup
  (resRes, resView) <- resForm lifterRes
  let lifterResulting = Lifter lifterName lifterAge lifterSex lifterAgeclass lifterWeightclass lifterWeight
                               lifterRaw <$> groupRes <*> resRes <*> pure lifterClub
  let widget = [whamlet|
         <div class="lifterRow">
           <span class="lifterName"> #{lifterName}
           ^{fvInput idView}
           ^{fvInput groupView}
           ^{resView}
  |]
  return (Entity <$> idRes <*> lifterResulting, widget)
  where
      fieldFormat = FieldSettings "" Nothing Nothing Nothing [("class", "tableText")]


liftersForm :: MeetState -> [Entity Lifter] -> Html -> MForm Handler (FormResult [Entity Lifter], Widget)
liftersForm meetState eLifterList extra = do
  list <- forM eLifterList lifterForm
  let reslist = fmap fst list :: [FormResult (Entity Lifter)]
  let res0 = (catMaybes $ map formEval reslist) :: [Entity Lifter]
  let viewList =  fmap snd list :: [Widget] --Liste der Widgets der einzelnen Lifter Formulare holen und mit Linebreak trennen
  let widgetsAndLifter = L.groupBy (\(l1,_) (l2,_) -> lifterGroup l1 == lifterGroup l2) $ zip lifterList viewList :: [[(Lifter,Widget)]]
  let combineWidgets1 l = F.foldl' (\w1 (_,w2) -> (w1 >> w2)) ([whamlet|
                                                              <div .gruppenBezeichner>
                                                                  Gruppe #{lifterGroup $ P.fst $ P.head l}
                                                            |]) l :: Widget --Gruppe ausgeben
  let combinedWidgets = F.foldl' (>>) ([whamlet|
                                      |]) (map combineWidgets1 widgetsAndLifter) :: Widget
                                      -- Liste zu einem Widget zusammenfügen und extra ding an den Anfang setzen
  let framedFrom = ([whamlet|
                #{extra}
                <div id="lifterForm">
                  <div id="lifterFormHeaderRow">
                    <span id="lifterNameHeader" class="lifterFormHeader"> Name
                    <span id="lifterGroupHeader" class="lifterFormHeader"> Gruppe
                    <span class="lifterAttemptHeader lifterFormHeader"> Versuch 1
                    <span class="lifterAttemptHeader lifterFormHeader"> Versuch 2
                    <span class="lifterAttemptHeaderEnd lifterFormHeader"> Versuch 3
                  ^{combinedWidgets}
              |])
  if (elem Nothing (map formEval reslist))
  then return (FormMissing, framedFrom)
  else return (pure res0, framedFrom)

  where
    lifterList = sortBy (cmpLifterGroupAndOrder meetState) $ map (\(Entity _ l) -> l) eLifterList
    formEval :: FormResult a -> Maybe a -- Eingegebenen Wert aus dem Formresult Funktor 'herausholen'
    formEval (FormSuccess s) = Just s
    formEval _ = Nothing

postAdminR :: Handler Html
postAdminR = do
  meetState <- getCurrMeetStateFromDB
  let groupNr = meetStateCurrGroupNr meetState
  ((result, _), _) <- runFormPost csvForm
  case result of
      FormSuccess (FileForm info) ->  handleFile (fileContentType info) (fileSource info)
      _ -> do
          lifters <- getELiftersFromDB
          ((res,_),_) <- runFormPost $ liftersForm meetState lifters
          case res of
              FormSuccess lifterList -> do
                  backup <- runDB $ selectList ([] :: [Filter Lifter]) []
                  backVersion <- getLatestBackupVersion
                  case backVersion of
                    Nothing -> backupLifter (map entityVal backup) 0
                    Just x -> backupLifter (map entityVal backup) (x+1)
                  let filteredLifterList = filter ((==) groupNr . lifterGroup . fromEntity) lifterList
                  let todo = [runDB $ updateWhere [LifterId ==. lId] [LifterGroup =. lifterGroup, LifterRes =. lifterRes]
                              | (Entity lId Lifter {..}) <-filteredLifterList] :: [Handler ()]
                  sequence_ todo
                  truncBackupHistory
                  -- update Frontends
                  pushDataToChannel (meetState, fromEntities lifterList)
                  getAdminR
              FormFailure (t:_) -> defaultLayout $ [whamlet| Error #{t} |]
              _ -> do
                  ((res',_),_) <- runFormPost $ meetStateForm meetState
                  case res' of
                      FormSuccess ms@(MeetState {..}) -> do
                        runDB $ updateWhere ([] :: [Filter MeetState]) [ MeetStateCurrGroupNr =. meetStateCurrGroupNr
                                                                       , MeetStateCurrDiscipline =. meetStateCurrDiscipline]
                        pushDataToChannel (ms, fromEntities lifters)
                        getAdminR

                      FormFailure (t:_) -> defaultLayout $ [whamlet| Error #{t} |]

                      _ -> error "FormError"

  where
    handleFile :: Text ->  ConduitT () ByteString  (ResourceT IO) () -> Handler Html
    handleFile typ rawFile |typ=="text/csv" = do
                                                csv <- liftIO $ parseCSV rawFile
                                                case fromNullable csv of
                                                  Just csvNonEmpty ->
                                                    let dataSet = sequenceA $ lifterParse <$> tail csvNonEmpty in
                                                    case dataSet of
                                                      (ARight datas) ->
                                                        do
                                                          let startGroupNr = P.head $ sort $ fmap lifterGroup datas
                                                          resetDB
                                                          _ <- runDB $ insertMany datas -- Insert CSV
                                                          _ <- runDB $ insert $
                                                               emptyMeetState { meetStateCurrGroupNr = startGroupNr }
                                                          getAdminR

                                                      (ALeft es) ->
                                                        invalidArgs es

                                                  _ -> error "CSV File empty"

                           |otherwise       = defaultLayout $
                                                  [whamlet| Please supply a correct CSV File! Your file was #{typ}|]


parseCSV :: ConduitT () ByteString (ResourceT IO) () -> IO [Row Text]
parseCSV rawFile =
    runResourceT $ runConduit $
    rawFile .| intoCSV defCSVSettings .| sinkList --defCSVSettings means , seperator and " to enclose fields

lifterParse :: Row Text -> ApplEither [Text] Lifter
lifterParse r@[name,age,sex,aclass,wclass,weight,raw,flight,club] =
    Lifter name age <$> safeRead sex      <*> safeRead aclass <*> safeRead wclass
                    <*> safeRead weight   <*> safeRead raw    <*> safeRead flight
                    <*> pure emptyResults <*> pure club
  where
    safeRead :: (Read a, Show a) => Text -> ApplEither [Text] a
    safeRead s = case P.reads $ T.unpack s of
                   [(t, "")] -> pure t
                   t         -> ALeft . pure $ "Error reading " ++ s ++ " in str " ++ T.intercalate ", " r
                                            ++ " result: "  ++ (T.pack $  show t)

lifterParse input = error ("Wrong number of entries in: " ++ show input)

getUndoR :: Handler Html
getUndoR = do
             restoreBackup
             redirect AdminR


-- LATEX EXPORT

getLatexTemplate :: IO Text
getLatexTemplate = fmap pack $ P.readFile latexTemplate

-- contentText -> klasse -> lifters -> out
composeClass :: Text -> Text -> Text -> Text
composeClass input cl lifters = T.replace "LIFTERS" lifters $ T.replace "KLASSE" cl input

-- START -> END -> input -> Text
getInnerText :: Text -> Text -> Text -> Text
getInnerText start end input = T.strip content
  where
    content' = let [_,x] = T.splitOn start input in x --anfang cutten
    content = let (x:_) = T.splitOn end content' in x -- ende cutten

getContentText :: Text -> Text
getContentText = getInnerText "CONTENTSTART" "CONTENTEND"

getLifterText :: Text -> Text
getLifterText = getInnerText "LIFTERSTART" "LIFTEREND"

-- Lifter -> lifterText -> place -> output
createLifter :: Text -> Lifter -> Int -> Text
createLifter inp l@(Lifter {..}) pl = F.foldl' (P.flip (P.$)) inp actions
  where
    actions :: [Text -> Text]
    actions = [ \src -> replaceAllCaptures TOP replWeight $ src *=~ attemptRegex
              , \src -> replaceAllCaptures TOP replGood $ src *=~ goodRegex] ++
              (map (\(a,b) -> T.replace a b)
                [("NAME", lifterName )
                ,("AGE", lifterAge )
                ,("BW", pack $ show $ lifterWeight )
                --,("ATTEMPT1",showAttempt $ lifterAttemptDL1Weight )
                --,("ATTEMPT2",showAttempt $ lifterAttemptDL2Weight )
                --,("ATTEMPT3",showAttempt $ lifterAttemptDL3Weight )
                --,("GOOD1", goodLift $ lifterAttemptDL1Success )
                --,("GOOD2", goodLift $ lifterAttemptDL2Success )
                --,("GOOD3", goodLift $ lifterAttemptDL3Success )
                ,("WILKS", calcWilks l)
                ,("PLACE", showPlacing l pl)
                ,("CLUB", escapeForLatex $ lifterClub )
                ,("TOTAL", showTotal l)])
    attemptRegex                   = [re|ATTEMPT_${d}([A-Za-z0-9]*)_${n}([1-3]{1})|]
    goodRegex                      = [re|GOOD_${d}([A-Za-z0-9]*)_${n}([1-3]{1})|]
    captureID                      = IsCaptureName . CaptureName
    replWeight                     = replaceFunc showAttemptWeight
    replGood                       = replaceFunc showGoodLift
    replaceFunc f m _ _            = do
      discName <- captureTextMaybe (captureID "d") m
      attNr    <- captureTextMaybe (captureID "n") m
      let disc = getDisciplineFromLifter discName l
      case attNr of
        "1" -> Just $ f $ att1 disc
        "2" -> Just $ f $ att2 disc
        "3" -> Just $ f $ att3 disc
        _ -> Nothing

showTotal :: Lifter -> Text
showTotal l = case getTotalLifter l of
                Just t -> pack $ show t
                Nothing -> "D.Q."

escapeForLatex :: Text -> Text
escapeForLatex = T.replace "&" "\\&"

showPlacing :: Lifter -> Int -> Text -- Check if bombout
showPlacing l pl = case getTotalLifter l of
                     Just _ -> pack $ show $ pl
                     Nothing -> "D.Q."

displ :: Weight -> Text
displ = pack . show

showAttemptWeight :: Attempt -> Text
showAttemptWeight (Success w) = displ w
showAttemptWeight (Fail w)    = displ w
showAttemptWeight (Todo w)    = displ w
showAttemptWeight _           = ""

showGoodLift :: Attempt -> Text
showGoodLift (Success _) = "1"
showGoodLift (Fail _)    = "-1"
showGoodLift _           = "0"

calcWilks :: Lifter -> Text
calcWilks l = pack $ show $ ((flip (/)) 1000 :: Double -> Double) $
                fromIntegral $ (round :: Double -> Int) $
                (*) 1000 $ ((fromRational $ wilks * total) :: Double)
  where
    am :: Rational
    am = -216.0475144
    bm :: Rational
    bm = 16.2606339
    cm :: Rational
    cm = -0.002388645
    dm :: Rational
    dm = -0.00113732
    em :: Rational
    em = 7.01863E-06
    fm :: Rational
    fm = -1.291E-08
    af :: Rational
    af = 594.31747775582
    bf :: Rational
    bf = -27.23842536447
    cf :: Rational
    cf = 0.82112226871
    df :: Rational
    df = -0.00930733913
    ef :: Rational
    ef =47.31582E-06
    ff :: Rational
    ff = -9.054E-08
    total = toRational $ case getTotalLifter l of
                           Just x -> x
                           Nothing -> 0
    bw = toRational $ lifterWeight l
    wilks = case lifterSex l of
              Male -> 500/(am + bm*bw + cm*bw^(2::Int) + dm*bw^(3::Int) + em*bw^(4::Int) + fm*bw^(5::Int))
              Female -> 500/(af + bf*bw + cf*bw^(2::Int) + df*bw^(3::Int) + ef*bw^(4::Int) + ff*bw^(5::Int))

createKlasse :: (Bool, Sex, Ageclass, Weightclass) -> Text
createKlasse (raw,sex,aclass,wclass) = T.intercalate ", " [showRaw,showSex,printPrettyAgeclass aclass, pack $ show wclass]
  where
    showRaw = case raw of
                True -> "ohne Ausrüstung"
                False -> "mit Ausrüstung"
    showSex = case sex of
                Male -> "Männlich"
                Female -> "Weiblich"


-- Alles um WEBSTART und WEBEND herum
getWrapper :: Text -> (Text, Text)
getWrapper input = let [before,y] = T.splitOn "WEBSTART" input in
                     let [_,after] = T.splitOn "WEBEND" y in (before,after)

liftersWithPlacings :: [Lifter] -> [[(Int,Lifter)]]
liftersWithPlacings lifters = map (zip [1..]) (liftersGrouped lifters)

liftersGrouped :: [Lifter] -> [[Lifter]]
liftersGrouped lifters = map (L.sortBy (cmpLifterTotalAndBw)) $
                             L.groupBy (\l1 l2 -> (lifterRaw l1, lifterSex l1, lifterAgeclass l1, lifterWeightclass l1) ==
                        (lifterRaw l2, lifterSex l2, lifterAgeclass l2, lifterWeightclass l2)) liftersSorted
  where
    liftersSorted = L.sortBy (\l1 l2 -> compare (lifterAgeclass l1, lifterSex l1, lifterWeightclass l1, lifterRaw l1)
                             (lifterAgeclass l2, lifterSex l2, lifterWeightclass l2, lifterRaw l2)) lifters

getTableR :: Handler TypedContent
getTableR = do
              input <- liftIO getLatexTemplate
              let contentText = getContentText input
              let lifterText = getLifterText input
              let mkClass = composeClass contentText
              liftersFromDB <- getLiftersFromDB
              let liftersTexts = map (T.strip . unlines . map (\(pl,l) -> createLifter lifterText l pl)) $ liftersWithPlacings liftersFromDB :: [Text]
              let classAndLifters = zip (map ((\l -> createKlasse (lifterRaw l, lifterSex l, lifterAgeclass l, lifterWeightclass l)) . P.head)
                                     (liftersGrouped liftersFromDB))
                                      liftersTexts :: [(Text,Text)] -- [(Klassentext,Liftertext)]
              let classBlocks = map (P.uncurry mkClass) classAndLifters :: [Text]
              --return $ TypedContent "text/plain" $ toContent $ let (wbefore,wafter) = getWrapper input in wbefore ++ (unlines classBlocks) ++ wafter
              return $ TypedContent "application/x-latex" $ toContent $ let (wbefore,wafter) = getWrapper input in wbefore ++ unlines classBlocks ++ wafter
