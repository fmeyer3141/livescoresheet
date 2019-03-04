{-# LANGUAGE OverloadedStrings #-}

module MeetState where

import Data.Text

import MeetTypes

data MeetState = MeetState {
    currDiscipline :: Text,
    meetType :: MeetType,
    currGroup :: Int
  }
