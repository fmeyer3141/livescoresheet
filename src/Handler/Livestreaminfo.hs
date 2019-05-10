{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Livestreaminfo where

import Import
import Yesod.WebSockets
import Misc

sendLivestreamInfo :: FrontendMessage -> Maybe Value
sendLivestreamInfo (LifterLiveStreamMessage v) = Just v
sendLivestreamInfo (JuryResultMessage v True)  = Just v
sendLivestreamInfo _                           = Nothing

getLivestreaminfoR :: Handler Html
getLivestreaminfoR = do
    webSockets $ dataSocket sendLivestreamInfo
    defaultLayout $ do
      setTitle "Livestreaminfo"
      $(widgetFile "livestreaminfo")
      $(widgetFile "show_jury_on_frontend")
