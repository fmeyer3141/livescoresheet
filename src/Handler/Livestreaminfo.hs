{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Livestreaminfo where

import Import
import Yesod.WebSockets

import Scoresheetlogic
import Misc

computeLivestreamInfo :: FrontendMessage -> Maybe Value
computeLivestreamInfo (LifterUpdate (ms, lifters)) =
    getNextLifterInGroup ms lifters >>= (\l -> getLivestreamInfo ms l lifters)

computeLivestreamInfo _                             = Nothing

getLivestreaminfoR :: Handler Html
getLivestreaminfoR = do
    webSockets $ dataSocket computeLivestreamInfo
    defaultLayout $ do
      setTitle "Livestreaminfo"
      $(widgetFile "livestreaminfo")
