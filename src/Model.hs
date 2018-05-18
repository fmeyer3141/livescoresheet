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

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


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
      "attemptDL1Weight" .= lifterAttemptDL1Weight,
      "attemptDL1Success" .= lifterAttemptDL1Success,
      "attemptDL2Weight" .= lifterAttemptDL2Weight,
      "attemptDL2Success" .= lifterAttemptDL2Success,
      "attemptDL3Weight" .= lifterAttemptDL3Weight,
      "attemptDL3Success" .= lifterAttemptDL3Success,
      "club" .= lifterClub
      ]
