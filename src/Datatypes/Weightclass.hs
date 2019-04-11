{-# LANGUAGE TemplateHaskell #-}

module Weightclass where

import Database.Persist.TH
import Prelude as P
import Data.Char
import Data.List.Extra

data Weightclass = MinusClass Int | PlusClass Int
    deriving (Eq,Ord)
derivePersistField "Weightclass"

instance Show Weightclass where
  show (MinusClass x) = "-" ++ show x
  show (PlusClass x)  = show x ++ "+"

instance Read Weightclass where
  readsPrec _  ('-':rest)  = let (weight,rest2) = P.span isDigit rest in
                              [(MinusClass (read weight),rest2)]

  readsPrec _  ('+':rest)  = let (weight,rest2) = P.span isDigit rest in
                              [(PlusClass (read weight),rest2)]

  readsPrec _ inp = let inp' = trim inp in
                    let (x:_) = inp' in
                    case isDigit x of
                      True ->
                        case span isDigit inp of
                          (digits, '+':rest2) -> [(PlusClass (read digits), rest2)]
                          _                   -> []

                      _    -> []
  readsPrec _ _ = error "Weightclass Format"

