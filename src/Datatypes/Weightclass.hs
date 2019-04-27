{-# LANGUAGE TemplateHaskell #-}

module Weightclass where

import Database.Persist.TH
import Prelude as P
import Data.Char
import Data.List.Extra

data Weightclass = Minusclass Int | Plusclass Int
    deriving (Eq,Ord)
derivePersistField "Weightclass"

weightclassFromDB :: Int -> Weightclass
weightclassFromDB w = if w >= 0 then Plusclass w else Minusclass (-w)

weightclassToDB :: Weightclass -> Int
weightclassToDB (Minusclass w) = -w
weightclassToDB (Plusclass  w) = w

instance Show Weightclass where
  show (Minusclass x) = "-" ++ show x
  show (Plusclass  x) = show x ++ "+"

instance Read Weightclass where
  readsPrec _  ('-':rest)  = let (weight,rest2) = P.span isDigit rest in
                              [(Minusclass (read weight),rest2)]

  readsPrec _  ('+':rest)  = let (weight,rest2) = P.span isDigit rest in
                              [(Plusclass (read weight),rest2)]

  readsPrec _ inp = let inp' = trim inp in
                    let (x:_) = inp' in
                    case isDigit x of
                      True ->
                        case span isDigit inp of
                          (digits, '+':rest2) -> [(Plusclass (read digits), rest2)]
                          _                   -> []

                      _    -> []
