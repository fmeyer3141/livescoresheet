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

import Data.CSV.Conduit
import Scoresheetlogic
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Prelude as P

newtype FileForm = FileForm
    { fileInfo :: FileInfo }

myLifter :: [Lifter]
myLifter = [Lifter {lifterName = "Equipment Chetah", lifterAge = 20, lifterSex = Male, lifterAgeclass = Junior, lifterWeightclass = "120", lifterWeight = 300.0, lifterRaw = False, lifterGroup = 10, lifterAttemptDL1Weight = Nothing, lifterAttemptDL1Success = Nothing, lifterAttemptDL2Weight = Nothing, lifterAttemptDL2Success = Nothing, lifterAttemptDL3Weight = Nothing, lifterAttemptDL3Success = Nothing}]

getLiftersFromDB :: Handler [Lifter]
getLiftersFromDB = do
    dataSet <- runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter])
    return [lifter | (Entity _ lifter) <- dataSet]

getCurrGroupNrFromDB :: Handler Int
getCurrGroupNrFromDB = do
    dataFromDB <- runDB $ selectList ([] :: [Filter CurrGroupNr]) []
    return $ P.head [groupNr | (Entity _ (CurrGroupNr groupNr))<-dataFromDB]

groupNrForm :: Int -> AForm Handler Int
groupNrForm g = areq intField "GroupNr: " $ Just g

getAdminR :: Handler Html
getAdminR = do
    (formWidget, formEnctype) <- generateFormPost csvForm
    lifters <- getLiftersFromDB
    groupNr <- getCurrGroupNrFromDB
    let lifters' = sortBy (cmpLifterGroupAndTotal groupNr) lifters
    (lifterformWidget, lifterformEnctype) <- generateFormPost $ liftersForm lifters'
    (groupNrformWidget, groupNrformEnctype) <- generateFormPost $ renderDivs $ groupNrForm groupNr 

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


liftersForm :: [Lifter] -> Html -> MForm Handler (FormResult [Lifter], Widget) --TODO lifterList sortieren nach Gruppen und dann Total
liftersForm lifterList extra = do
    list <- forM lifterList lifterForm
    let reslist = fmap fst list :: [FormResult Lifter]
    let res0 = (map (\(Just x) -> x) $ filter (/= Nothing) $ map formEval reslist) :: [Lifter]
    let viewList =  fmap snd list :: [Widget] --Liste der Widgets der einzelnen Lifter Formulare holen und mit Linebreak trennen
    let widgetsAndLifter = L.groupBy (\(l1,_) (l2,_) -> lifterGroup l1 == lifterGroup l2) $ zip lifterList viewList :: [[(Lifter,Widget)]]
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
        formEval :: FormResult a -> Maybe a -- Eingegebenen Wert aus dem Formresult Funktor 'herausholen'
        formEval (FormSuccess s) = Just s
        formEval _ = Nothing

postAdminR :: Handler Html
postAdminR = do
    ((result, _), _) <- runFormPost csvForm
    case result of
        FormSuccess (FileForm info) ->  handleFile (fileContentType info) (fileSource info)
        _ -> do
            lifters <- getLiftersFromDB
            ((res,_),_) <- runFormPost $ liftersForm lifters
            case res of
                FormSuccess lifterList -> do
                    let todo = [runDB $ updateWhere
                               [LifterName ==. n]
                               [LifterGroup =. g, LifterAttemptDL1Weight =. d1w, LifterAttemptDL1Success =. d1s, LifterAttemptDL2Weight =. d2w,
                                LifterAttemptDL2Success =. d2s, LifterAttemptDL3Weight =. d3w, LifterAttemptDL3Success =. d3s]
                                | (Lifter n _ _ _ _ _ _ g d1w d1s d2w d2s d3w d3s)<-lifterList] :: [Handler ()]
                    sequence_ todo
                    getAdminR
                FormFailure (t:_) -> defaultLayout $ [whamlet| Error #{t} |]
                _ -> do
                    groupNr <- getCurrGroupNrFromDB
                    ((res',_),_) <- runFormPost $ renderDivs $ groupNrForm groupNr
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
                                                        _ <- runDB $ deleteWhere ([] :: [Filter Lifter]) -- Truncate table
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
lifterParse [name,age,sex,aclass,wclass,weight,raw,flight] =
    Lifter name (P.read $ T.unpack age) (P.read $ T.unpack sex) (P.read $ T.unpack aclass) (T.unpack wclass) (P.read $ T.unpack weight)
        (P.read $ T.unpack raw) (P.read $ T.unpack flight) w s w s w s
    where
        w = Nothing
        s = Nothing
lifterParse input = error ("Somethings wrong with the CSV-file with " ++ show input)

