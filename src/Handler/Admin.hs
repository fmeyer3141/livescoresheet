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

import Data.List

newtype FileForm = FileForm
    { fileInfo :: FileInfo }

getAdminR :: Handler Html
getAdminR = do 
    (formWidget, formEnctype) <- generateFormPost csvForm

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
        FormSuccess (FileForm info) ->  defaultLayout $ handleFile (fileContentType info) (fileSource info)
        FormFailure (t:_) -> defaultLayout $ [whamlet| #{t}|] 
        _ -> defaultLayout $ [whamlet| Error|]
    where
        handleFile :: (Yesod site) => Text ->  Source (ResourceT IO) ByteString -> WidgetT site IO () 
        handleFile typ rawFile |typ=="text/csv" = do
                                                    table <- liftIO $ parseCSV rawFile
                                                    let a = show table
                                                    [whamlet| Test <p> #{a}|]
                               |otherwise       = [whamlet| Please supply a correct CSV File! Your file was #{typ}|]


getColumn :: [Row Text] -> Int -> [Text]
getColumn rows i = fmap (!! i) rows

parseCSV :: Source (ResourceT IO) ByteString -> IO [Row Text]
parseCSV rawFile = 
    runResourceT $ rawFile $= 
    intoCSV defCSVSettings $$ --defCSVSettings means , seperator and " to enclose fields
    sinkList
