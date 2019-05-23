{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Urkunden where

import Import
import Handler.Admin
import ManageScoresheetState
import qualified Data.Text as T
import qualified Prelude as P
import Ageclass
import PackedHandler
import Scoresheetlogic (showTotal, getTotalLifter)

getUrkundenR :: Handler TypedContent
getUrkundenR = do
  liftersFromDB <- atomicallyUnpackHandler getLiftersFromDB
  let lifters = liftersWithPlacings liftersFromDB
  let lifterCSV =  (++) identifiers $ T.concat $ P.map (T.concat .
                   map
                   (\(pl,l@Lifter {..}) -> T.intercalate "," [ lifterName, lifterClub, printPrettyAgeclass $ lifterAgeclass, pack $ show lifterWeightclass
                                                             , showPlacing l pl, showTotal l, showWilks lifterSex (getTotalLifter l) lifterWeight
                                                             , pack $ show lifterRaw, pack $ show lifterSex] ++ "\n"))
                   lifters -- :: Text
  return $ TypedContent "text/csv" $ toContent lifterCSV--(T.pack $ show $ lifterCSV)

   where
     identifiers = "Name,Club,Ageclass,Weightclass,Placing,Total,Wilks,Raw,Sex\n"
