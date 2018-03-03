-- @Sex.hs
{-# LANGUAGE TemplateHaskell #-}
module Sex where

import Database.Persist.TH
import Prelude

data Sex = Male | Female
    deriving (Show, Read, Eq)
derivePersistField "Sex"
