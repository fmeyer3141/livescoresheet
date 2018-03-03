-- @Ageclass.hs
{-# LANGUAGE TemplateHaskell #-}
module Ageclass where

import Database.Persist.TH
import Prelude

data Ageclass = Subjunior | Junior | Open | Master1 | Master2 | Master3 | Master4
    deriving (Show, Read, Eq)
derivePersistField "Ageclass"
