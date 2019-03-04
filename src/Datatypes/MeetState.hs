{-# LANGUAGE OverloadedStrings #-}

module MeetState where

import Data.Text
import THAppl

import MeetTypes

data MeetState = MeetState {
    currDiscipline :: Text,
    meetType :: MeetType,
    currGroup :: Int
  }
