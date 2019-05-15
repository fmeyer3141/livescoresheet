{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Stecker where

import Import
import Yesod.WebSockets

import Misc

test :: [(Plate,Int)] -- Jede Scheibe einmal und zwei 25er zum Layout testen
test = zip [Plate25,Plate20 .. Plate1_25] (2 : repeat 1)

sendSteckerData :: FrontendMessage -> Maybe Value
sendSteckerData (SteckerInfoMessage v) = Just v
sendSteckerData _                      = Nothing

getSteckerR :: Handler Html
getSteckerR = do
    webSockets $ dataSocket sendSteckerData
    defaultLayout $ do
      setTitle "Steckeranzeige"
      $(widgetFile "stecker")
