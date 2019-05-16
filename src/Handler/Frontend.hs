{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Frontend where

import Import
import Yesod.WebSockets

import Misc

import Text.Julius (RawJS (..))

sendFrontendData :: FrontendMessage -> Maybe Value
sendFrontendData (LifterFrontendMessage v)  = Just v
sendFrontendData (JuryResultMessage v True) = Just v
sendFrontendData _                          = Nothing

getFrontendR :: Bool -> Handler Html
getFrontendR showJury = do
    addHeader "Access-Control-Allow-Origin" "*"
    webSockets $ dataSocket sendFrontendData
    defaultLayout $ do
      setTitle "Scoresheet"
      let juryCode = if showJury then "showJury(data[0], data[1]);" else "" :: Text
      let discNames = fst <$> meetType :: [Text]
      $(widgetFile "show_jury_on_frontend")
      $(widgetFile "formatweight")
      $(widgetFile "frontend")

getBeamerR :: Handler Html
getBeamerR = redirect $ FrontendR True

getFrontendDefR :: Handler Html
getFrontendDefR = redirect $ FrontendR False
