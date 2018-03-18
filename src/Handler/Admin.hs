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
import Data.Text (Text)
import qualified Data.Text as T

import qualified Prelude as P

newtype FileForm = FileForm
    { fileInfo :: FileInfo }


myLifter :: [Lifter]
myLifter = [Lifter {lifterName = "Equipment Chetah", lifterAge = 20, lifterSex = Male, lifterAgeclass = Junior, lifterWeightclass = "120", lifterWeight = 300.0, lifterRaw = False, lifterGroup = 10, lifterAttemptDL1Weight = Nothing, lifterAttemptDL1Success = Nothing, lifterAttemptDL2Weight = Nothing, lifterAttemptDL2Success = Nothing, lifterAttemptDL3Weight = Nothing, lifterAttemptDL3Success = Nothing}]

getLiftersFromDB :: Handler [Lifter]
getLiftersFromDB = do
    dataSet <- runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter])
    return [lifter | (Entity _ lifter) <- dataSet]

getAdminR :: Handler Html
getAdminR = do
    (formWidget, formEnctype) <- generateFormPost csvForm
    lifters <- getLiftersFromDB
    (lifterformWidget, lifterformEnctype) <- generateFormPost $ liftersForm lifters

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
                #{lifterName lifter}
                ^{fvInput groupView}
                ^{fvInput lifterAttemptDL1WeightView}
                ^{fvInput lifterAttemptDL1SuccessView}
                ^{fvInput lifterAttemptDL2WeightView}
                ^{fvInput lifterAttemptDL2SuccessView}
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
    let res0 = map formEval reslist :: [Lifter]
    let viewList = Import.intersperse [whamlet| <br> |] $ fmap snd list :: [Widget] --Liste der Widgets der einzelnen Lifter Formulare holen und mit Linebreak trennen
    let combinedWidgets = P.foldl (>>) ([whamlet| #{extra} |]) viewList :: Widget -- Liste zu einem Widget zusammenfügen und extra ding an den Anfang setzen
    return (pure res0, combinedWidgets)

    where
        formEval :: FormResult a -> a -- Eingegebenen Wert aus dem Formresult Funktor 'herausholen'
        formEval (FormSuccess s) = s
        formEval _ = error "Error in Formeval"

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
                _ -> error "Error while submitting Form"

    where
        handleFile :: Text ->  Source (ResourceT IO) ByteString -> Handler Html
        handleFile typ rawFile |typ=="text/csv" = do
                                                    (_:table) <- liftIO $ parseCSV rawFile --remove captions
                                                    let dataSet = fmap lifterParse table :: [Lifter]
                                                    do
                                                        _ <- runDB $ deleteWhere ([] :: [Filter Lifter]) -- Truncate table
                                                        _ <- runDB $ insertMany dataSet -- Insert CSV
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

