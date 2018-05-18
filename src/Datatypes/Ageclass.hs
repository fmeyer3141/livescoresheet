-- @Ageclass.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Ageclass where

import Database.Persist.TH
import Prelude
import Data.Text

data Ageclass = Subjunior | Junior | Open | Master1 | Master2 | Master3 | Master4
    deriving (Show, Read, Eq, Ord)
derivePersistField "Ageclass"

printPrettyAgeclass :: Ageclass -> Text
printPrettyAgeclass Subjunior = "Jugend"
printPrettyAgeclass Junior = "Junioren"
printPrettyAgeclass Open = "Aktive"
printPrettyAgeclass Master1 = "AK 1"
printPrettyAgeclass Master2 = "AK 2"
printPrettyAgeclass Master3 = "AK 3"
printPrettyAgeclass Master4 = "AK 4"
