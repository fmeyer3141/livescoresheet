{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

data FileForm = FileForm
    { fileInfo :: FileInfo }

getAdminR :: Handler Html
getAdminR = do 
    (formWidget, formEnctype) <- generateFormPost sampleForm

    defaultLayout $ do
        setTitle "Welcome to the mighty Scoresheet"
        $(widgetFile "adminpage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Wählen Sie die Anmeldungsdatei aus."

postAdminR :: Handler Html
postAdminR = error "Not yet implemented: postAdminR"
