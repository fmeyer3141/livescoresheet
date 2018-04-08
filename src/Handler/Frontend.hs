{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Frontend where

import Import
import Scoresheetlogic
import Prelude (head)
import Handler.Admin

getFrontendR :: Handler TypedContent 
getFrontendR = selectRep $ do
  provideRep $ 
    defaultLayout $ do 
      setTitle "Scoresheet"
      $(widgetFile "frontend")
  provideRep $ do
     lifters <- getLiftersFromDB
     groupNr <- getCurrGroupNrFromDB
     let lifterGroupList = filter (\l -> (lifterGroup l) == groupNr) lifters :: [Lifter]
     let nextLifters = sortBy (cmpLifterOrder) lifterGroupList
     let nextLifter = Prelude.head $ nextLifters
     let (currAgeClass,currWeightClass) = (lifterAgeclass nextLifter, lifterWeightclass nextLifter)
     let nextLiftersFiltered = filter (\l -> 4 /= nextAttemptNr l) nextLifters
     let nextLiftersOutput = map (\l -> (lifterName l, nextWeight l $ nextAttemptNr l, nextAttemptNr l)) nextLiftersFiltered
     return $ toJSON $ nextLiftersOutput 