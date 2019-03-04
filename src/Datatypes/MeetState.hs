{-# LANGUAGE OverloadedStrings #-}

module MeetState where

import Import

data MeetState = MeetState {
    currDiscipline :: Text,
    currGroup :: Int
  }
