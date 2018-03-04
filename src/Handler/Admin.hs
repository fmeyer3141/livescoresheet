{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Data.CSV.Conduit
import Data.Text (Text)
import qualified Data.Text as T

import Prelude (read)

newtype FileForm = FileForm
    { fileInfo :: FileInfo }

getAdminR :: Handler Html
getAdminR = do 
    (formWidget, formEnctype) <- generateFormPost csvForm
    dataSet <- runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter]) 
    let databaseContent = show dataSet  -- do it but do it pretty

    defaultLayout $ do
        setTitle "Welcome to the mighty Scoresheet"
        $(widgetFile "adminpage")

csvForm :: Form FileForm
csvForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Wählen Sie die Anmeldungsdatei aus."

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

