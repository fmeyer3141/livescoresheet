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
{-# LANGUAGE DataKinds                  #-}

module Model ( module MeetTypesTH
             , module THApplStage1
             , module THApplStage2
             , module ApplEither
             , module Model)
  where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Sex
import Ageclass
import ApplEither
import Weightclass
import THApplStage1
import THApplStage2
import MeetTypesTH
import Data.Maybe
import qualified Prelude as P
import Control.Lens (view)

import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

data RefereePlaces  = PLeft | PMain | PRight deriving (Show, Eq, Read)

instance PathPiece RefereePlaces where
  toPathPiece = T.pack . show
  fromPathPiece s =
    case P.reads $ T.unpack s of
      (p, ""):_ -> Just p
      _         -> Nothing

data RefereeDecision (p :: RefereePlaces) = RefereeDecision { red :: Bool, blue :: Bool, yellow :: Bool }

data RefereeResult = RefereeResult { left  :: Maybe (RefereeDecision 'PLeft)
                                   , main  :: Maybe (RefereeDecision 'PMain)
                                   , right :: Maybe (RefereeDecision 'PRight) }

resultList :: Results -> [Discipline]
resultList res = (\(_,l) -> (view l) res) <$> meetType

getBestAttempt :: Discipline -> Maybe Double
getBestAttempt = P.maximum . fmap attemptWeight . attemptsAsList

getDisciplineFromLifter :: Text -> Lifter -> Discipline
getDisciplineFromLifter n Lifter {..} = fromJust $ P.lookup n $ zip disciplineNames (resultList $ lifterRes)
  where
    disciplineNames = fstMeetType <$> meetType

emptyMeetState :: MeetState
emptyMeetState =
  MeetState { meetStateCurrDiscipline = fstMeetType . P.head $ meetType
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
  toJSON res = toJSON $ (\(n,l) -> (n, (view l) res)) <$> meetType
