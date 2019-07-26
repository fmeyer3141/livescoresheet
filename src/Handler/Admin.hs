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
import Yesod.WebSockets

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

import Data.Time.Clock.POSIX
import           Text.RE.Replace
import           Text.RE.TDFA.Text

import Control.Lens ((^.))
import Control.Lens.Setter
import System.Random.Shuffle
import ManageScoresheetState
import PackedHandler (atomicallyUnpackHandler)
import Misc

import Control.Monad.Logger

latexTemplate :: AppSettings -> String
latexTemplate settings = "latexexport/" ++ appLatexTemplateName settings

newtype FileForm = FileForm
    { fileInfo :: FileInfo }

meetStateForm :: [GroupNr] -> MeetState -> Html -> MForm Handler (FormResult MeetState, Widget)
meetStateForm gNrs MeetState {..} = renderDivs $
                                    MeetState <$> areq (selectFieldList list) "CurrDiscipline" (Just meetStateCurrDiscipline)
                                              <*> areq (selectFieldList gList) "GroupNr: " (Just meetStateCurrGroupNr)
  where
    double a = (a,a)
    list :: [(Text,Text)]
    list = map (double . fst) meetType
    gList :: [(Text, Int)]
    gList = (\g -> (T.pack $ show g, g)) <$> gNrs

sendKariData :: FrontendMessage -> Maybe Value
sendKariData (JuryResultMessage v _) = Just v
sendKariData _                       = Nothing

getAdminR :: Handler Html
getAdminR = do
    webSockets $ dataSocket sendKariData
    addHeader "Cache-Control" "no-cache, no-store, must-revalidate"
    maid <- maybeAuthId
    (formWidget, formEnctype) <- generateFormPost csvForm

    (meetState, eLifters, groupNrs) <- atomicallyUnpackHandler $ do
      ms <- getCurrMeetStateFromDB
      eLs  <- getELiftersInGroupFromDB (meetStateCurrGroupNr ms)
      gNrs <- getGroupNrsFromDB
      pure (ms, eLs, gNrs)

    (lifterformWidget, lifterformEnctype) <- generateFormPost $ liftersForm meetState eLifters
    (meetStateFormWidget, meetStateEnctype) <- generateFormPost $ meetStateForm groupNrs meetState

    defaultLayout $ do
        setTitle "Welcome to the mighty Scoresheet"
        let currDisc = meetStateCurrDiscipline meetState
        let discNames = fst <$> meetType :: [Text]
        $(widgetFile "adminpage")

csvForm :: Form FileForm
csvForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Wählen Sie die Anmeldungsdatei aus."

attemptForm :: Text -> Attempt -> MForm Handler (FormResult Attempt, Widget)
attemptForm dDescr att = do
  (timeRes,timeView)      <- mreq hiddenFieldDouble fieldFormat $ Just (attGetChangedTime att)
  (weightRes, weightView) <- mopt doubleField fieldFormat (Just $ attemptWeight att)
  (succRes, succView)     <- mreq (selectFieldList succType) "" (Just $ attemptToModifier att)
  let attRes = createAttempt <$> weightRes <*> succRes <*> timeRes
  return (attRes, [whamlet|
                    <div .attempt .discCell#{dDescr}>
                      ^{fvInput timeView} ^{fvInput weightView} ^{fvInput succView} |])

  where
    fieldFormat = FieldSettings "" Nothing Nothing Nothing [("class", "tableText")]
    succType :: [(Text,LiftModifier)]
    succType = [("Todo", MTodo), ("Good", MGood), ("Fail", MFail), ("Skip", MSkip)]
    createAttempt weight suc t =
      case (weight, suc) of
        (Just w, MGood) -> Attempt (Success w) t
        (Just w, MFail) -> Attempt (Fail w)    t
        (Just w, MTodo) -> Attempt (Todo w)    t
        (_, MSkip)      -> Attempt Skip        t
        (_, _)          -> Attempt Unset       t
    hiddenFieldDouble :: Field Handler Double
    hiddenFieldDouble =
      doubleField { fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
        $newline never
        <input type="hidden" id="#{theId}" name="#{name}" *{attrs} value="#{either id  (T.pack . show) $ val}">
      |] }

disciplineForm :: Text -> Discipline -> MForm Handler (FormResult Discipline, Widget)
disciplineForm dDescr Discipline { .. } =
  do
    (att1f, att1View) <- attemptForm dDescr att1
    (att2f, att2View) <- attemptForm dDescr att2
    (att3f, att3View) <- attemptForm dDescr att3
    let widget = [whamlet| ^{att1View} ^{att2View} ^{att3View}|]
    let disciplineRes = Discipline <$> att1f <*> att2f <*> att3f
    return (disciplineRes, widget)

resForm :: Results -> MForm Handler (FormResult Results, Widget)
resForm res =
  do
    discForms <- forM meetType (\(n, Lens'NT l) -> disciplineForm n (res ^. l))
    let widgets = fmap snd discForms

    let discWidgets = F.foldl' (>>) [whamlet| |] widgets
    let resChanges =  zipWith resChangesf (fst <$> discForms) meetType
    let resRes = F.foldl' (\r f -> f r) (Just res) resChanges
    return $ case resRes of
      Just r -> (pure r, discWidgets)
      _      -> (FormFailure ["Error parsing results form"], discWidgets)

    where
      resChangesf :: FormResult Discipline -> (Text, Lens'NT Results Discipline) -> Maybe Results -> Maybe Results
      resChangesf (FormSuccess d) (_,Lens'NT l) = fmap $ l .~ d
      resChangesf _               _             = id

lifterForm :: (Key Lifter', Lifter) -> MForm Handler (FormResult (Key Lifter', Lifter), Widget)
lifterForm (lId, Lifter {..}) = do
  (idRes,idView) <- mreq hiddenField fieldFormat $ Just lId
  (weightRes, weightView) <- mreq doubleField fieldFormatAdditional $ Just lifterWeight
  (resRes, resView) <- resForm lifterRes
  (groupRes, groupView) <- mreq intField fieldFormatAdditional $ Just lifterGroup
  (oocRes, oocView) <- mreq checkBoxField fieldFormatAdditional $ Just lifterOutOfCompetition
  (weightclassRes, weightclassView) <- mreq intField fieldFormatAdditional $
                                         Just (weightclassToInt lifterWeightclass)
  let lifterResulting = Lifter lifterName lifterLot lifterAge lifterSex lifterAgeclass
                               <$> (weightclassFromInt <$> weightclassRes) <*> oocRes   <*> weightRes
                               <*> pure lifterRaw                          <*> groupRes <*> resRes
                               <*> pure lifterClub
  let widget = [whamlet|
         <div class="lifterRow">
           <span class="lifterName"> #{lifterName}
           ^{fvInput idView}
           ^{fvInput weightView}
           ^{resView}
           ^{fvInput oocView}
           ^{fvInput groupView}
           ^{fvInput weightclassView}
  |]
  return ((,) <$> idRes <*> lifterResulting, widget)
  where
    fieldFormat           = FieldSettings "" Nothing Nothing Nothing [("class", "tableText")]
    fieldFormatAdditional = FieldSettings "" Nothing Nothing Nothing [("class", "additionalChangesCell tableText")]


liftersForm :: MeetState -> [(Key Lifter', Lifter)] -> Html -> MForm Handler (FormResult [(Key Lifter', Lifter)], Widget)
liftersForm meetState eLifterList extra = do
  let sortedList = sortBy (cmpLifterGroupAndOrder meetState `on` snd) eLifterList
  list <- forM sortedList lifterForm
  -- Combine Widgets and Lifters and then group by liftergroups
  let liftersAndWidgets = L.groupBy ((==) `on` (lifterGroup . fst)) $
                            zip (snd <$> sortedList) (snd <$> list)
  let combineWidgets1 l = [whamlet|
                            <div .gruppenBezeichner>
                              Gruppe #{lifterGroup $ fst $ unsafeHead l}
                          |] *> F.sequenceA_ (snd <$> l) :: Widget --Gruppe ausgeben
  let combinedWidgets = F.sequenceA_ (map combineWidgets1 liftersAndWidgets) :: Widget
  let framedForm = [whamlet|
                #{extra}
                <div #lifterForm>
                  <div #lifterFormHeaderRow>
                    <span #lifterNameHeader .lifterFormHeader> Name
                    <span #lifterGroupHeader .lifterFormHeader .additionalChangesHead> Gewicht
                    $forall d <- map fst meetType
                      <span .lifterAttemptHeader .lifterFormHeader .discHead#{d}> #{d} 1
                      <span .lifterAttemptHeader .lifterFormHeader .discHead#{d}> #{d} 2
                      <span .lifterAttemptHeaderEnd .lifterFormHeader .discHead#{d}> #{d} 3
                    <span #lifterGroupHeader .lifterFormHeader .additionalChangesHead> OOC
                    <span #lifterGroupHeader .lifterFormHeader .additionalChangesHead> Gruppe
                    <span #lifterGroupHeader .lifterFormHeader .additionalChangesHead> Gew. Klasse
                  ^{combinedWidgets}
              |]
  pure (sequenceA $ fst <$> list, framedForm)

postCSVFormR :: Handler Html
postCSVFormR = do
  ((result, _), _) <- runFormPost csvForm
  case result of
      FormSuccess (FileForm info) -> handleFile (fileContentType info) (fileSource info) *> redirect AdminR
      FormFailure fs -> invalidArgs fs
      _              -> defaultLayout [whamlet| Form is missing|]
  where
    handleFile :: Text ->  ConduitT () ByteString  (ResourceT IO) () -> Handler Html
    handleFile typ rawFile
      |typ=="text/csv" = do
        csv <- liftIO $ parseCSV rawFile
        time <- realToFrac <$> liftIO getPOSIXTime
        case fromNullable csv of
          Just csvNonEmpty ->
            let dataSet = sequenceA $ lifterParse time <$> tail csvNonEmpty in
            case dataSet of
              (ARight datas) ->
                do
                  let startGroupNr = unsafeHead $ sort $ lifterGroup <$> datas
                  lotNumbers <- liftIO $ shuffleM [1 .. length datas]
                  let liftersWithLots = zipWith (\n l -> l {lifterLot = n}) lotNumbers datas
                  atomicallyUnpackHandler $
                    initialSetupDB liftersWithLots startGroupNr
                  logInfoN "load csv"
                  getAdminR

              (ALeft es) ->
                invalidArgs es

          _ -> invalidArgs ["CSV-Form is missing"]

      |otherwise       = defaultLayout [whamlet| Please supply a correct CSV File! Your file was #{typ}|]

postLifterFormR :: Handler Html
postLifterFormR = do
  meetState <- atomicallyUnpackHandler getCurrMeetStateFromDB
  let groupNr = meetStateCurrGroupNr meetState
  lifters <- atomicallyUnpackHandler $ getELiftersInGroupFromDB groupNr
  ((res,_),_) <- runFormPost $ liftersForm meetState lifters
  case res of
      FormSuccess lifterList ->
          atomicallyUnpackHandler (updateLiftersInDB lifterList) *> redirect AdminR
      FormFailure fs -> invalidArgs fs
      _ -> invalidArgs ["Lifter-Form is missing"]

postMeetStateFormR :: Handler Html
postMeetStateFormR = do
  (meetState, groupNrs) <- atomicallyUnpackHandler $ (,) <$> getCurrMeetStateFromDB <*> getGroupNrsFromDB
  ((res,_),_) <- runFormPost $ meetStateForm groupNrs meetState
  case res of
      FormSuccess ms ->
        atomicallyUnpackHandler (updateMeetState ms)
        *> redirect AdminR
      FormFailure fs -> invalidArgs fs

      _ -> invalidArgs ["MeetState-Form is missing"]

parseCSV :: ConduitT () ByteString (ResourceT IO) () -> IO [Row Text]
parseCSV rawFile =
    runResourceT $ runConduit $
    rawFile .| intoCSV defCSVSettings .| sinkList --defCSVSettings means , seperator and " to enclose fields

lifterParse :: AttemptTime -> Row Text -> ApplEither [Text] Lifter
lifterParse time r@[name,age,sex,aclass,wclass,weight,raw,flight,club,outOfCompetition] =
    Lifter name 0 <$> safeRead age             <*> safeRead sex              <*> safeRead aclass
                  <*> safeRead wclass          <*> safeRead outOfCompetition <*> safeRead weight
                  <*> safeRead raw             <*> safeRead flight           <*> pure (emptyResults time)
                  <*> pure club
  where
    safeRead :: (Read a, Show a) => Text -> ApplEither [Text] a
    safeRead s = case P.reads $ T.unpack s of
                   [(t, "")] -> pure t
                   t         -> ALeft . pure $ "Error reading " ++ s ++ " in str " ++ T.intercalate ", " r
                                            ++ " result: "  ++ T.pack (show t)

lifterParse _ input = error ("Wrong number of entries in: " ++ show input)

getUndoR :: Handler Html
getUndoR = do
             atomicallyUnpackHandler restoreBackup
             redirect AdminR


-- LATEX EXPORT

getLatexTemplate :: AppSettings -> IO Text
getLatexTemplate = readFileUtf8 . latexTemplate

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
createLifter inp l@Lifter {..} pl = F.foldl' (flip ($)) inp actions
  where
    actions :: [Text -> Text]
    actions = [ \src -> replaceAllCaptures TOP replWeight $ src *=~ attemptRegex
              , \src -> replaceAllCaptures TOP replGood $ src *=~ goodRegex] ++
                  map (uncurry T.replace)
                    [("NAME", lifterName )
                    ,("AGE", pack $ show lifterAge )
                    ,("BW", pack $ show lifterWeight )
                    ,("WILKS", showWilks lifterSex (getTotalLifter l) lifterWeight)
                    ,("PLACINGINFO", showPlacingInfo l)
                    ,("PLACE", showPlacing l pl)
                    ,("CLUB", escapeForLatex lifterClub)
                    ,("TOTAL", showTotal l)]
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

escapeForLatex :: Text -> Text
escapeForLatex = T.replace "&" "\\&"

showPlacing :: Lifter -> Int -> Text
showPlacing l pl = case getTotalLifter l of
                     Just _ -> pack (show pl)
                     Nothing -> ""

displ :: Weight -> Text
displ = pack . show

showAttemptWeight :: Attempt -> Text
showAttemptWeight (Attempt (Success w) _) = displ w
showAttemptWeight (Attempt (Fail w)    _) = displ w
showAttemptWeight (Attempt (Todo w)    _) = displ w
showAttemptWeight _             = ""

showPlacingInfo :: Lifter -> Text
showPlacingInfo l@Lifter{..} =
  case (lifterOutOfCompetition, getTotalLifter l) of
    (True, _)        -> "1" -- a.K.
    (False, Just _)  -> "0"
    (False, Nothing) -> "-1" --DQ

showGoodLift :: Attempt -> Text
showGoodLift (Attempt (Success _) _) = "1"
showGoodLift (Attempt (Fail _)    _) = "-1"
showGoodLift _                       = "0"

showWilks :: Sex -> Maybe Double -> Double -> Text
showWilks _   Nothing      _          = ""
showWilks sex (Just total) bodyweight = pack $ show $ ((flip (/)) 1000 :: Double -> Double) $
                                          fromIntegral $ (round :: Double -> Int) $
                                          (*) 1000 $ ((fromRational $ wilks * tot) :: Double)
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
    tot = toRational total
    bw = toRational bodyweight
    wilks = case sex of
              Male -> 500/(am + bm*bw + cm*bw^(2::Int) + dm*bw^(3::Int) + em*bw^(4::Int) + fm*bw^(5::Int))
              Female -> 500/(af + bf*bw + cf*bw^(2::Int) + df*bw^(3::Int) + ef*bw^(4::Int) + ff*bw^(5::Int))

createKlasse :: (Bool, Sex, Ageclass, Weightclass) -> Text
createKlasse (raw,sex,aclass,wclass) = T.intercalate ", " [showRaw,showSex,printPrettyAgeclass aclass, pack $ show wclass]
  where
    showRaw = if raw then "ohne Ausrüstung" else "mit Ausrüstung"
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
liftersGrouped lifters = map (L.sortBy cmpLifterPlacing) $
                             L.groupBy (\l1 l2 -> (lifterRaw l1, lifterSex l1, lifterAgeclass l1, lifterWeightclass l1) ==
                        (lifterRaw l2, lifterSex l2, lifterAgeclass l2, lifterWeightclass l2)) liftersSorted
  where
    liftersSorted = L.sortBy (\l1 l2 -> compare (lifterAgeclass l1, lifterSex l1, lifterWeightclass l1, lifterRaw l1)
                             (lifterAgeclass l2, lifterSex l2, lifterWeightclass l2, lifterRaw l2)) lifters

getTableR :: Handler TypedContent
getTableR = do
  templateName <- appSettings <$> getYesod
  input <- liftIO $ getLatexTemplate templateName
  let contentText = getContentText input
  let lifterText = getLifterText input
  let mkClass = composeClass contentText
  liftersFromDB <- atomicallyUnpackHandler getLiftersFromDB
  let liftersTexts = map (T.strip . unlines . map (\(pl,l) -> createLifter lifterText l pl)) $ liftersWithPlacings liftersFromDB :: [Text]
  let classAndLifters = zip (map ((\l -> createKlasse (lifterRaw l, lifterSex l, lifterAgeclass l, lifterWeightclass l)) . unsafeHead)
                         (liftersGrouped liftersFromDB))
                          liftersTexts :: [(Text,Text)] -- [(Klassentext,Liftertext)]
  let classBlocks = map (uncurry mkClass) classAndLifters :: [Text]
  --return $ TypedContent "text/plain" $ toContent $ let (wbefore,wafter) = getWrapper input in wbefore ++ (unlines classBlocks) ++ wafter
  return $ TypedContent "application/x-latex" $ toContent $ let (wbefore,wafter) = getWrapper input in wbefore ++ unlines classBlocks ++ wafter
