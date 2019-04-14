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

computeSteckerData :: (MeetState, [Lifter]) -> Value
computeSteckerData (ms, lifters) =
  let nextLifter = getNextLifterInGroup ms lifters in
  toJSON $ do
    l <- nextLifter
    nextWeightl <- nextWeight ms l $ nextAttemptNr ms l
    return ( lifterName l, lifterClub l, meetStateCurrDiscipline ms
           , meetStateCurrGroupNr ms, nextAttemptNr ms l, nextWeightl, getPlates nextWeightl)
    -- return ( lifterName l, lifterClub l, meetStateCurrDiscipline ms
    --        , meetStateCurrGroupNr ms, nextAttemptNr ms l, nextWeightl, test )

getSteckerR :: Handler Html
getSteckerR = do
    webSockets $ dataSocket computeSteckerData
    defaultLayout $ do
      setTitle "Steckeranzeige"
      $(widgetFile "stecker")
