{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module MeetTypes where

import ClassyPrelude.Yesod
import Database.Persist.TH
import Data.Text


data Attempt = Attempt { weight :: Maybe Double, success :: Maybe Bool } deriving (Show, Read, Eq)
data Discipline = Discipline { att1 :: Attempt, att2 :: Attempt, att3 :: Attempt} deriving (Show, Read, Eq)
data Results = Results { resultList :: [Discipline]} deriving (Show, Read, Eq)
derivePersistField "Results"

newtype MeetType = MeetType [(Text, Discipline)]

instance ToJSON Attempt where
  toJSON Attempt {..} = object [ "weight" .= show weight, "success" .= show success]

instance ToJSON Discipline where
  toJSON Discipline {..} = object [ "att1" .= toJSON att1, "att2" .= toJSON att2, "att3" .= toJSON att3]

attemptsAsList Discipline {..} = [att1,att2,att3]
