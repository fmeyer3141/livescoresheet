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
import qualified Data.List as L

getFrontendR :: Handler TypedContent
getFrontendR = selectRep $ do
  provideRep $
    defaultLayout $ do
      setTitle "Scoresheet"
      $(widgetFile "frontend")
  provideRep $ do
     lifters <- getLiftersFromDB
     groupNr <- getCurrGroupNrFromDB
     let lifterGroupList = filter (\l -> lifterGroup l == groupNr) lifters :: [Lifter]
     let nextLifters = sortBy cmpLifterOrder lifterGroupList
     let nextLifter = Prelude.head nextLifters
     let c = getClass nextLifter
     let nextLiftersFiltered = filter (\l -> Nothing /= nextWeight l (nextAttemptNr l)) nextLifters
     let nextLiftersOutput = map (\l -> (lifterName l, nextWeight l $ nextAttemptNr l, nextAttemptNr l)) nextLiftersFiltered
     let liftersGroupedByClass = map (sortBy cmpLifterTotalAndBw) $ L.groupBy (\l l' -> getClass l == getClass l') $ sortBy (compareLifterClass c) lifters
     let liftersOverview = map (map $ \l -> (isNext (listToMaybe nextLiftersFiltered) l,l,calcWilks l)) liftersGroupedByClass:: [[(Bool,Lifter,Text)]]
     -- The Bool indicates if the Lifter is the next
     return $ toJSON (liftersOverview, nextLiftersOutput)
     where
        isNext :: Maybe Lifter -> Lifter -> Bool
        isNext (Just l') l = l == l'
        isNext _ _         = False

