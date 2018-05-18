{-# LANGUAGE TemplateHaskell #-}

module Weightclass where

import Database.Persist.TH
import Prelude as P
import Data.Char

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
  readsPrec _ inp@(x:_) = case isDigit x of
                              True -> case span isDigit inp of
                                (digits, '+':rest2) -> [(PlusClass (read digits), rest2)]
                                _ -> error "No sign"
                              _    -> error "Not starting with a digit or a sign"
  readsPrec _ _ = error "Weightclass Format"

