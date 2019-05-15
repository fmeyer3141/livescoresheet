-- @Sex.hs
{-# LANGUAGE TemplateHaskell #-}
module Sex where

import Database.Persist.TH

data Sex = Male | Female
    deriving (Show, Read, Eq, Ord)
derivePersistField "Sex"

sexFromDB :: Bool -> Sex
sexFromDB True  = Female
sexFromDB False = Male

sexToDB :: Sex -> Bool
sexToDB Female = True
sexToDB Male   = False
