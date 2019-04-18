{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Urkunden where

import Import
import Handler.Admin
import ManageScoresheetState
import qualified Data.Text as T
import qualified Prelude as P
import Ageclass
import PackedHandler

getUrkundenR :: Handler TypedContent
getUrkundenR = do
  liftersFromDB <- atomicallyUnpackHandler getLiftersFromDB
  let lifters = liftersWithPlacings liftersFromDB
  let lifterCSV =  (++) identifiers $ T.concat $ P.map (T.concat .
                   map
                   (\(pl,l) -> T.intercalate "," [lifterName l, lifterClub l, printPrettyAgeclass $ lifterAgeclass l,
                               pack (show $ lifterWeightclass l), showPlacing l pl,
                               showTotal l, calcWilks l, pack $ show $ lifterRaw l, pack $ show $ lifterSex l] ++ "\n"))
                   lifters -- :: Text
  return $ TypedContent "text/csv" $ toContent lifterCSV--(T.pack $ show $ lifterCSV)

   where
     identifiers = "Name,Club,Ageclass,Weightclass,Placing,Total,Wilks,Raw,Sex\n"
