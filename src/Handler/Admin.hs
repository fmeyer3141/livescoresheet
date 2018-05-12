{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Sex
import Ageclass

import qualified Data.Text as T

import Data.CSV.Conduit
import Scoresheetlogic
import Data.Text (Text)
import qualified Data.List as L
import qualified Prelude as P

latexTemplate :: String
latexTemplate = "latexexport/template.tex"

newtype FileForm = FileForm
    { fileInfo :: FileInfo }

myLifter :: [Lifter]
myLifter = [Lifter {lifterName = "Equipment Chetah", lifterAge = "20", lifterSex = Male, lifterAgeclass = Junior, lifterWeightclass = "120", lifterWeight = 300.0, lifterRaw = False, lifterGroup = 10, lifterAttemptDL1Weight = Nothing, lifterAttemptDL1Success = Nothing, lifterAttemptDL2Weight = Nothing, lifterAttemptDL2Success = Nothing, lifterAttemptDL3Weight = Nothing, lifterAttemptDL3Success = Nothing, lifterClub = "Aachener Kraftsport e.V."}]

getLiftersFromDB :: Handler [Lifter]
getLiftersFromDB = do
    dataSet <- runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter])
    return [lifter | (Entity _ lifter) <- dataSet]

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
                      let insertList = [LifterBackup v na ag se acl wcl we ra gr adl1w adl1s adl2w adl2s adl3w adl3s cl | (Lifter na ag se acl wcl we ra gr adl1w adl1s adl2w adl2s adl3w adl3s cl)<-lifterList] :: [LifterBackup]
                      _ <- runDB $ insertMany insertList
                      return ()

truncBackupHistory :: Handler ()
truncBackupHistory = do
                       backupDB <- runDB $ selectList [] [Desc LifterBackupVersion]
                       let backupVersions = group $ map (lifterBackupVersion . entityVal) backupDB
                       case (length backupVersions) >= 10 of
                         True -> do
                                   let deleteAfterVersion = P.head $ backupVersions P.!! 10
                                   runDB $ deleteWhere [LifterBackupVersion <. deleteAfterVersion]
                         False -> return ()

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
                              let insertBack = [Lifter na ag se acl wcl we ra gr adl1w adl1s adl2w adl2s adl3w adl3s cl | (LifterBackup _ na ag se acl wcl we ra gr adl1w adl1s adl2w adl2s adl3w adl3s cl)<-map entityVal backup]
                              _ <- runDB $ insertMany insertBack
                              return ()

getCurrGroupNrFromDB :: Handler Int
getCurrGroupNrFromDB = do
    dataFromDB <- runDB $ selectList ([] :: [Filter CurrGroupNr]) []
    return $ if ((length dataFromDB) > 0) then P.head [groupNr | (Entity _ (CurrGroupNr groupNr))<-dataFromDB]
    else 0

--groupNrForm :: Int -> AForm Handler Int
--groupNrForm g = areq intField "GroupNr: " $ Just g

groupNrForm :: Int -> Form Int
groupNrForm g = renderBootstrap3 BootstrapBasicForm $
                    areq intField "GroupNr: " (Just g)

getAdminR :: Handler Html
getAdminR = do
    maid <- maybeAuthId
    (formWidget, formEnctype) <- generateFormPost csvForm
    lifters <- getLiftersFromDB
    groupNr <- getCurrGroupNrFromDB
    (lifterformWidget, lifterformEnctype) <- generateFormPost $ liftersForm groupNr lifters
    (groupNrformWidget, groupNrformEnctype) <- generateFormPost $ groupNrForm groupNr

    defaultLayout $ do
        setTitle "Welcome to the mighty Scoresheet"
        $(widgetFile "adminpage")

csvForm :: Form FileForm
csvForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Wählen Sie die Anmeldungsdatei aus."

lifterForm :: Lifter -> MForm Handler (FormResult Lifter, Widget)
lifterForm lifter = do
    (groupRes, groupView) <- mreq intField fieldFormat (Just (lifterGroup lifter))
    (lifterAttemptDL1WeightRes, lifterAttemptDL1WeightView) <- mopt doubleField fieldFormat (Just $ lifterAttemptDL1Weight lifter)
    (lifterAttemptDL1SuccessRes, lifterAttemptDL1SuccessView) <- mreq (selectFieldList succType) "" (Just $ lifterAttemptDL1Success lifter)
    (lifterAttemptDL2WeightRes, lifterAttemptDL2WeightView) <- mopt doubleField fieldFormat (Just $ lifterAttemptDL2Weight lifter)
    (lifterAttemptDL2SuccessRes, lifterAttemptDL2SuccessView) <- mreq (selectFieldList succType) "" (Just $ lifterAttemptDL2Success lifter)
    (lifterAttemptDL3WeightRes, lifterAttemptDL3WeightView) <- mopt doubleField fieldFormat (Just $ lifterAttemptDL3Weight lifter)
    (lifterAttemptDL3SuccessRes, lifterAttemptDL3SuccessView) <- mreq (selectFieldList succType) "" (Just $ lifterAttemptDL3Success lifter)
    let lifterRes = Lifter (lifterName lifter)
                           (lifterAge lifter)
                           (lifterSex lifter)
                           (lifterAgeclass lifter)
                           (lifterWeightclass lifter)
                           (lifterWeight lifter)
                           (lifterRaw lifter) <$> groupRes <*> lifterAttemptDL1WeightRes
                                                           <*> lifterAttemptDL1SuccessRes
                                                           <*> lifterAttemptDL2WeightRes
                                                           <*> lifterAttemptDL2SuccessRes
                                                           <*> lifterAttemptDL3WeightRes
                                                           <*> lifterAttemptDL3SuccessRes
                                                           <*> pure (lifterClub lifter)
    let widget =
            [whamlet|
                <div class="lifterRow">
                  <span class="lifterName"> #{lifterName lifter}
                  ^{fvInput groupView}
                  <div class="attempt">
                    ^{fvInput lifterAttemptDL1WeightView}
                    ^{fvInput lifterAttemptDL1SuccessView}
                  <div class="attempt">
                    ^{fvInput lifterAttemptDL2WeightView}
                    ^{fvInput lifterAttemptDL2SuccessView}
                  <div class="attempt">
                    ^{fvInput lifterAttemptDL3WeightView}
                    ^{fvInput lifterAttemptDL3SuccessView}
            |] -- TODO Gruppennummer als class oder so ausgeben für schönere Optik
    return (lifterRes, widget)
    where
        fieldFormat = FieldSettings "" Nothing Nothing Nothing [("class", "tableText")]
        succType :: [(Text, Maybe Bool)]
        succType = [("ToDo", Nothing), ("Good", Just True), ("Fail", Just False)]


liftersForm :: Int -> [Lifter] -> Html -> MForm Handler (FormResult [Lifter], Widget)
liftersForm groupNr lifterList extra = do
    list <- forM lifterList' lifterForm
    let reslist = fmap fst list :: [FormResult Lifter]
    let res0' = (map formEval reslist) :: [Maybe Lifter]
    let res0 = (map (\(Just x) -> x) $ filter (/= Nothing) $ res0') :: [Lifter]
    let viewList =  fmap snd list :: [Widget] --Liste der Widgets der einzelnen Lifter Formulare holen und mit Linebreak trennen
    let widgetsAndLifter = L.groupBy (\(l1,_) (l2,_) -> lifterGroup l1 == lifterGroup l2) $ zip lifterList' viewList :: [[(Lifter,Widget)]]
    let combineWidgets1 l = P.foldl (\w1 (_,w2) -> (w1 >> w2)) ([whamlet|
                                                                <div .gruppenBezeichner>
                                                                    Gruppe #{lifterGroup $ P.fst $ P.head l}
                                                              |]) l :: Widget --Gruppe ausgeben
    let combinedWidgets = P.foldl (>>) ([whamlet|
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
        lifterList' = sortBy (cmpLifterGroupAndOrder groupNr) lifterList
        formEval :: FormResult a -> Maybe a -- Eingegebenen Wert aus dem Formresult Funktor 'herausholen'
        formEval (FormSuccess s) = Just s
        formEval _ = Nothing

postAdminR :: Handler Html
postAdminR = do
    groupNr <- getCurrGroupNrFromDB
    ((result, _), _) <- runFormPost csvForm
    case result of
        FormSuccess (FileForm info) ->  handleFile (fileContentType info) (fileSource info)
        _ -> do
            lifters <- getLiftersFromDB
            ((res,_),_) <- runFormPost $ liftersForm groupNr lifters
            case res of
                FormSuccess lifterList -> do
                    backup <- runDB $ selectList ([] :: [Filter Lifter]) []
                    backVersion <- getLatestBackupVersion
                    case backVersion of
                      Nothing -> backupLifter (map entityVal backup) 0
                      Just x -> backupLifter (map entityVal backup) (x+1)
                    let filteredLifterList = filter ((==) groupNr . lifterGroup) lifterList
                    let todo = [runDB $ updateWhere
                               [LifterName ==. n]
                               [LifterGroup =. g, LifterAttemptDL1Weight =. d1w, LifterAttemptDL1Success =. d1s, LifterAttemptDL2Weight =. d2w,
                                LifterAttemptDL2Success =. d2s, LifterAttemptDL3Weight =. d3w, LifterAttemptDL3Success =. d3s]
                                | (Lifter n _ _ _ _ _ _ g d1w d1s d2w d2s d3w d3s _)<-filteredLifterList] :: [Handler ()]
                    sequence_ todo
                    truncBackupHistory
                    getAdminR
                FormFailure (t:_) -> defaultLayout $ [whamlet| Error #{t} |]
                _ -> do
                    ((res',_),_) <- runFormPost $ groupNrForm groupNr
                    case res' of
                        FormSuccess gNr -> do
                                              _ <- runDB $ updateWhere ([] :: [Filter CurrGroupNr]) [CurrGroupNrGroupNr =. gNr]
                                              getAdminR
                        FormFailure (t:_) -> defaultLayout $ [whamlet| Error #{t} |]
                        _ -> error "FormError"

    where
        handleFile :: Text ->  Source (ResourceT IO) ByteString -> Handler Html
        handleFile typ rawFile |typ=="text/csv" = do
                                                    (_:table) <- liftIO $ parseCSV rawFile --remove captions
                                                    let dataSet = fmap lifterParse table :: [Lifter]
                                                    let startGroupNr = P.head $ sort $ fmap lifterGroup dataSet :: Int
                                                    do
                                                        _ <- runDB $ deleteWhere ([] :: [Filter Lifter]) -- Truncate tables
                                                        _ <- runDB $ deleteWhere ([] :: [Filter LifterBackup])
                                                        _ <- runDB $ deleteWhere ([] :: [Filter CurrGroupNr])
                                                        _ <- runDB $ insertMany dataSet -- Insert CSV
                                                        _ <- runDB $ insert $ CurrGroupNr startGroupNr
                                                        getAdminR

                               |otherwise       = defaultLayout $
                                                      [whamlet| Please supply a correct CSV File! Your file was #{typ}|]


parseCSV :: Source (ResourceT IO) ByteString -> IO [Row Text]
parseCSV rawFile =
    runResourceT $ rawFile $=
    intoCSV defCSVSettings $$ --defCSVSettings means , seperator and " to enclose fields
    sinkList

lifterParse :: Row Text -> Lifter
lifterParse [name,age,sex,aclass,wclass,weight,raw,flight,club] =
    Lifter name age (P.read $ T.unpack sex) (P.read $ T.unpack aclass) (T.unpack wclass) (P.read $ T.unpack weight)
        (P.read $ T.unpack raw) (P.read $ T.unpack flight) w s w s w s club
    where
        w = Nothing
        s = Nothing
lifterParse input = error ("Somethings wrong with the CSV-file with " ++ show input)

getUndoR :: Handler Html
getUndoR = do
             restoreBackup
             redirect AdminR


-- LATEX EXPORT

getTemplate :: IO Text
getTemplate = fmap pack $ P.readFile latexTemplate

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
createLifter inp l pl = P.foldl (P.flip (P.$)) inp actions
  where
    actions :: [Text -> Text]
    actions = map (\(a,b) -> T.replace a b)
              [("NAME", lifterName l)
              ,("AGE", lifterAge l)
              ,("BW", pack $ show $ lifterWeight l)
              ,("ATTEMPT1",showAttempt $ lifterAttemptDL1Weight l)
              ,("ATTEMPT2",showAttempt $ lifterAttemptDL2Weight l)
              ,("ATTEMPT3",showAttempt $ lifterAttemptDL3Weight l)
              ,("GOOD1", goodLift $ lifterAttemptDL1Success l)
              ,("GOOD2", goodLift $ lifterAttemptDL2Success l)
              ,("GOOD3", goodLift $ lifterAttemptDL3Success l)
              ,("WILKS", calcWilks l)
              ,("PLACE", showPlacing l pl)
              ,("CLUB", escapeForLatex $ lifterClub l)
              ,("TOTAL", showTotal l)]

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

showAttempt :: Maybe Double -> Text
showAttempt (Just x) = pack $ show $ x
showAttempt Nothing  = "0.0"

goodLift :: Maybe Bool -> Text
goodLift (Just True)  = "1"
goodLift (Just False) = "-1"
goodLift _            = "0"

calcWilks :: Lifter -> Text
calcWilks l = pack $ show $ ((flip (/)) 100 :: Double -> Double) $
                fromIntegral $ (round :: Double -> Int) $
                (*) 100 $ ((fromRational $ wilks * total) :: Double)
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

createKlasse :: (Bool, Sex, Ageclass, String) -> Text
createKlasse (raw,sex,aclass,wclass) = T.intercalate ", " [showRaw,showSex,pack $ show aclass, pack $ wclass]
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


getTableR :: Handler TypedContent
getTableR = do
              input <- liftIO $ getTemplate
              let contentText = getContentText input
              let lifterText = getLifterText input
              let mkClass = composeClass contentText
              liftersFromDB <- getLiftersFromDB
              let liftersSorted = L.sortBy (\l1 l2 -> compare (lifterRaw l1, lifterSex l1, lifterAgeclass l1, lifterWeightclass l1)
                                          (lifterRaw l2, lifterSex l2, lifterAgeclass l2, lifterWeightclass l2)) liftersFromDB
              let liftersGrouped = map ( L.sortBy (cmpLifterTotalAndBw) ) $
                                          L.groupBy (\l1 l2 -> (lifterRaw l1, lifterSex l1, lifterAgeclass l1, lifterWeightclass l1) ==
                                          (lifterRaw l2, lifterSex l2, lifterAgeclass l2, lifterWeightclass l2)) liftersSorted :: [[Lifter]]
              let liftersWithPlacings = map (zip [1..]) liftersGrouped :: [[(Int,Lifter)]]
              let liftersTexts = map (T.strip . unlines . map (\(pl,l) -> createLifter lifterText l pl)) $ liftersWithPlacings :: [Text]
              let classAndLifters = zip (map ((\l -> createKlasse (lifterRaw l, lifterSex l, lifterAgeclass l, lifterWeightclass l)) . P.head) liftersGrouped)
                                      liftersTexts :: [(Text,Text)] -- [(Klassentext,Liftertext)]
              let classBlocks = map (\(a,b) -> mkClass a b) classAndLifters :: [Text]
              --return $ TypedContent "text/plain" $ toContent $ let (wbefore,wafter) = getWrapper input in wbefore ++ (unlines classBlocks) ++ wafter
              return $ TypedContent "application/x-latex" $ toContent $ let (wbefore,wafter) = getWrapper input in wbefore ++ (unlines classBlocks) ++ wafter
