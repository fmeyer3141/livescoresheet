{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Sex
import Ageclass
import Weightclass
import THApplStage1
import THApplStage2

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON Lifter where
  toJSON Lifter {..} =
    object [
        "name" .= lifterName,
        "age" .= lifterAge,
        "sex" .= (show lifterSex),
        "ageclass" .= (show lifterAgeclass),
        "weightclass" .= (show lifterWeightclass),
        "weight" .= lifterWeight,
        "raw" .= lifterRaw,
        "group" .= lifterGroup,
        "results" .= toJSON lifterRes,
        "club" .= lifterClub
      ]

instance ToJSON Results where
  toJSON res = toJSON $ (\(n,f) -> (n, f res)) <$> (unpackMeet meetType)
