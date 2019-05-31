{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.LiftersByLot (getLiftersByLotR) where

import Import
import ManageScoresheetState
import PackedHandler
import Scoresheetlogic

import qualified Data.Text as T

getLiftersByLotR :: Handler TypedContent
getLiftersByLotR = do
  lifters <- atomicallyUnpackHandler getLiftersFromDB
  let sorted = sortBy ((compare `on` lifterLot) .~. (compare `on` lifterGroup)) lifters
  let content = T.intercalate "\n" $ map lifterToCSV sorted
  let csv = csvHeader `T.append` content
  pure $ TypedContent "text/csv" $ toContent csv

  where
    csvHeader = "Name,Gruppe,Los\n"
    lifterToCSV :: Lifter -> Text
    lifterToCSV Lifter {..} = T.intercalate "," [lifterName, T.pack $ show lifterGroup, T.pack $ show lifterLot]
