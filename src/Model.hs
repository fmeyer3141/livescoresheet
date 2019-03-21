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
import MeetTypesTH
import Data.Maybe
import qualified Prelude as P

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

resultList :: Results -> [Discipline]
resultList res = (\(_,v,_) -> v res) <$> (unpackMeet meetType)

getBestAttempt :: Discipline -> Maybe Double
getBestAttempt = P.maximum . fmap attemptWeight . attemptsAsList

getDisciplineFromLifter :: Text -> Lifter -> Discipline
getDisciplineFromLifter n Lifter {..} = fromJust $ P.lookup n $ zip disciplineNames (resultList $ lifterRes)
  where
    disciplineNames = fst3 <$> (unpackMeet meetType)

emptyMeetState :: MeetState
emptyMeetState =
  MeetState { meetStateCurrDiscipline = fst3 . P.head $ unpackMeet meetType
            , meetStateCurrGroupNr = 0
            }

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
  toJSON res = toJSON $ (\(n,v,_) -> (n, v res)) <$> (unpackMeet meetType)
