{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Stecker where

import Import
import Yesod.WebSockets

import Scoresheetlogic
import Misc
import qualified Prelude as P

test :: [(Plate,Int)] -- Jede Scheibe einmal und zwei 25er zum Layout testen
test = zip [Plate25,Plate20 .. Plate1_25] (2 : P.repeat 1)

computeSteckerData :: FrontendMessage -> Maybe Value
computeSteckerData (LifterUpdate (ms, lifters)) =
  Just $ toJSON $ doubleMap getLifterInfo $ getNext2LiftersInGroup ms lifters
  where
    getLifterInfo ml =
      do
        l <- ml
        w <- nextWeight ms l
        return ( lifterName l, lifterClub l, meetStateCurrDiscipline ms
               , meetStateCurrGroupNr ms, nextAttemptNr ms l, w, getPlates w)

computeSteckerData _                             = Nothing


getSteckerR :: Handler Html
getSteckerR = do
    webSockets $ dataSocket computeSteckerData
    defaultLayout $ do
      setTitle "Steckeranzeige"
      $(widgetFile "stecker")
