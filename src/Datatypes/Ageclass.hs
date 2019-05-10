-- @Ageclass.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Ageclass where

import Prelude
import Data.Text

data Ageclass = Subjunior | Junior | Open | Master1 | Master2 | Master3 | Master4
    deriving (Show, Read, Eq, Ord)

ageclassFromDB :: Int -> Ageclass
ageclassFromDB 0 = Subjunior
ageclassFromDB 1 = Junior
ageclassFromDB 2 = Open
ageclassFromDB 3 = Master1
ageclassFromDB 4 = Master2
ageclassFromDB 5 = Master3
ageclassFromDB 6 = Master4
ageclassFromDB _ = error "DBError" --TODO sinnvolles handling

ageclassToDB :: Ageclass -> Int
ageclassToDB Subjunior = 0
ageclassToDB Junior    = 1
ageclassToDB Open      = 2
ageclassToDB Master1   = 3
ageclassToDB Master2   = 4
ageclassToDB Master3   = 5
ageclassToDB Master4   = 6

printPrettyAgeclass :: Ageclass -> Text
printPrettyAgeclass Subjunior = "Jugend"
printPrettyAgeclass Junior = "Junioren"
printPrettyAgeclass Open = "Aktive"
printPrettyAgeclass Master1 = "AK 1"
printPrettyAgeclass Master2 = "AK 2"
printPrettyAgeclass Master3 = "AK 3"
printPrettyAgeclass Master4 = "AK 4"
