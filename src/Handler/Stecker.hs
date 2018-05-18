{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Stecker where

import Import
import Scoresheetlogic
import Handler.Admin
import qualified Prelude as P

test :: [(Plate,Int)] -- Jede Scheibe einmal und zwei 25er
test = zip [Plate25,Plate20 .. Plate1_25] (2 : P.repeat 1)

getSteckerR :: Handler TypedContent
getSteckerR = selectRep $ do
  provideRep $
    defaultLayout $ do
      setTitle "Steckeranzeige"
      $(widgetFile "stecker")

  provideRep $ do
    lifters <- getLiftersFromDB
    groupNr <- getCurrGroupNrFromDB
    let nextLifter = getNextLifterInGroup lifters groupNr
    return $ toJSON $ do
      l <- nextLifter
      nextWeightl <- nextWeight l $ nextAttemptNr l
      return (lifterName l, lifterClub l, nextAttemptNr l, nextWeightl, getPlates nextWeightl)
      --return (lifterName l, lifterWeight l, lifterClub l, nextWeightl, test)
