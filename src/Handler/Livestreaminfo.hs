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

getLivestreaminfoR :: Handler Html
getLivestreaminfoR = undefined

-- computeSteckerData :: FrontendMessage -> Maybe Value
-- computeSteckerData (LifterUpdate (ms, lifters)) =
--   Just $ toJSON $ doubleMap (map $ getLifterAttemptInfo ms) $ getNext2LiftersInGroup ms lifters
--
-- computeSteckerData _                             = Nothing
--
--
-- getSteckerR :: Handler Html
-- getSteckerR = do
--     webSockets $ dataSocket computeSteckerData
--     defaultLayout $ do
--       setTitle "Steckeranzeige"
--       $(widgetFile "stecker")
