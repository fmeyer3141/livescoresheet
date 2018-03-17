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

import Prelude (read)

newtype FileForm = FileForm
    { fileInfo :: FileInfo }
myLifter :: Lifter
myLifter = Lifter {lifterName = "Equipment Chetah", lifterAge = 20, lifterSex = Male, lifterAgeclass = Junior, lifterWeightclass = "120", lifterWeight = 300.0, lifterRaw = False, lifterGroup = 10, lifterAttemptDL1Weight = Nothing, lifterAttemptDL1Success = Nothing, lifterAttemptDL2Weight = Nothing, lifterAttemptDL2Success = Nothing, lifterAttemptDL3Weight = Nothing, lifterAttemptDL3Success = Nothing}


getAdminR :: Handler Html
getAdminR = do 
    (formWidget, formEnctype) <- generateFormPost csvForm
    (lifterformWidget, lifterformEnctype) <- generateFormPost $ lifterForm myLifter 
    dataSet <- runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter]) 
    let databaseContent = show dataSet  -- do it but do it pretty

    defaultLayout $ do
        setTitle "Welcome to the mighty Scoresheet"
        $(widgetFile "adminpage")

csvForm :: Form FileForm
csvForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Wählen Sie die Anmeldungsdatei aus."


lifterForm :: [Lifter] -> Html -> MForm Handler (FormResult Lifter, Widget)
lifterForm lifterList extra = 
	do
        lifter<-lifterList
        do
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
    let widget = do
            [whamlet|
                #{extra}
                #{lifterName lifter}
                ^{fvInput groupView}
                ^{fvInput lifterAttemptDL1WeightView}
                ^{fvInput lifterAttemptDL1SuccessView}
                ^{fvInput lifterAttemptDL2WeightView}
                ^{fvInput lifterAttemptDL2SuccessView}
                ^{fvInput lifterAttemptDL3WeightView}
                ^{fvInput lifterAttemptDL3SuccessView}
            |]
    return (lifterRes, widget)
    where
        fieldFormat = FieldSettings "" Nothing Nothing Nothing [("class", "tableText")]
        succType :: [(Text, Maybe Bool)]
        succType = [("ToDo", Nothing), ("Good", Just True), ("Fail", Just False)]


postAdminR :: Handler Html
postAdminR = do
    ((result, _), _) <- runFormPost csvForm
    case result of
        FormSuccess (FileForm info) ->  handleFile (fileContentType info) (fileSource info)
        FormFailure (t:_) -> defaultLayout $ [whamlet| #{t}|] 
        _ -> defaultLayout $ [whamlet| Error|]
    where
        handleFile :: Text ->  Source (ResourceT IO) ByteString -> Handler Html 
        handleFile typ rawFile |typ=="text/csv" = do
                                                    (_:table) <- liftIO $ parseCSV rawFile --remove captions
                                                    let dataSet = fmap lifterParse table :: [Lifter]
                                                        b = show dataSet
                                                    do 
                                                        _ <- runDB $ deleteWhere ([] :: [Filter Lifter]) -- Truncate table
                                                        _ <- runDB $ insertMany dataSet -- Insert CSV
                                                        defaultLayout $ [whamlet| <p> #{b} <p>
                                                                                              <a href=@{AdminR}> Back |]

                               |otherwise       = defaultLayout $
                                                      [whamlet| Please supply a correct CSV File! Your file was #{typ}|]


parseCSV :: Source (ResourceT IO) ByteString -> IO [Row Text]
parseCSV rawFile = 
    runResourceT $ rawFile $= 
    intoCSV defCSVSettings $$ --defCSVSettings means , seperator and " to enclose fields
    sinkList

lifterParse :: Row Text -> Lifter
lifterParse [name,age,sex,aclass,wclass,weight,raw,flight] = 
    Lifter name (read $ T.unpack age) (read $ T.unpack sex) (read $ T.unpack aclass) (T.unpack wclass) (read $ T.unpack weight)
        (read $ T.unpack raw) (read $ T.unpack flight) w s w s w s
    where
        w = Nothing
        s = Nothing
lifterParse input = error ("Somethings wrong with the CSV-file with " ++ show input)

