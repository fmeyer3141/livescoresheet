{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Frontend where

import Import
import Yesod.WebSockets

import Data.Maybe (listToMaybe,isJust)
import qualified Data.List as L

import Scoresheetlogic
import Handler.Admin
import Misc

computeFrontendData :: (MeetState, [Lifter]) -> Value
computeFrontendData (ms, lifters) =
  do
     let groupNr = meetStateCurrGroupNr ms
     let lifterGroupList = filter (\l -> lifterGroup l == groupNr) lifters :: [Lifter]
     let nextLifters = sortBy (cmpLifterOrder ms) lifterGroupList
     let nextLiftersFiltered = filter (\l -> isJust $ nextWeight ms l) nextLifters
     let nextLiftersOutput = map (\l -> (lifterName l, nextWeight ms l, nextAttemptNr ms l))
                                 nextLiftersFiltered
     let mc = getClass <$> listToMaybe nextLiftersFiltered
     let liftersSortedByClass = case mc of
                                  (Just c) -> sortBy (cmpLifterClassPrio c) lifters
                                  Nothing  -> sortBy cmpLifterClass lifters
     let liftersGroupedByClass = map (sortBy cmpLifterTotalAndBw) $ L.groupBy (\l l' -> getClass l == getClass l') liftersSortedByClass
     let liftersOverview = map (map $ \l -> (isNext (listToMaybe nextLiftersFiltered) l,l,calcWilks l)) liftersGroupedByClass:: [[(Bool,Lifter,Text)]]
     -- The Bool indicates if the Lifter is the next
     toJSON (liftersOverview, nextLiftersOutput)
     where
        isNext :: Maybe Lifter -> Lifter -> Bool
        isNext (Just l') l = l == l'
        isNext _ _         = False

getFrontendR :: Handler Html
getFrontendR = do
    addHeader "Access-Control-Allow-Origin" "*"
    webSockets $ dataSocket computeFrontendData
    defaultLayout $ do
      setTitle "Scoresheet"
      $(widgetFile "frontend")
