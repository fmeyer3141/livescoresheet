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
import Sex
import Ageclass
import ApplEither
import Weightclass
import THApplStage1
import THApplStage2
import MeetTypesTH
import Data.Maybe (fromJust)
import qualified Prelude as P
import Control.Lens ((^.))

import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(databaseScheme) -- $(persistFileWith lowerCaseSettings "config/models")

type GroupNr = Int
type Placing = Int

data Lifter = Lifter { lifterName        :: !Text
                     , lifterAge         :: !Int
                     , lifterSex         :: !Sex
                     , lifterAgeclass    :: !Ageclass
                     , lifterWeightclass :: !Weightclass
                     , lifterWeight      :: !Double
                     , lifterRaw         :: !Bool
                     , lifterGroup       :: !GroupNr
                     , lifterRes         :: !Results
                     , lifterClub        :: !Text
                     } deriving (Show, Eq)

data LifterBackup = LifterBackup { lifterBackupLifter  :: !Lifter
                                 , lifterBackupVersion :: !Int    }

$(dbLifterConvFunctions)

fromLifterList :: [Lifter] -> [Lifter']
fromLifterList = map toLifter'

data RefereePlaces  = PLeft | PMain | PRight deriving (Show, Eq, Read)

instance PathPiece RefereePlaces where
  toPathPiece = T.pack . show
  fromPathPiece s =
    case P.reads $ T.unpack s of
      (p, ""):_ -> Just p
      _         -> Nothing

data RefereeDecision = RefereeDecision { red :: Bool, blue :: Bool, yellow :: Bool }

instance ToJSON RefereeDecision where
  toJSON RefereeDecision {..} = toJSON (red,blue,yellow)

instance Show RefereeDecision where
  show (RefereeDecision r b y) = show . filter snd $ zip (["r","b","y"] :: [[Char]]) [r,b,y]

data RefereeResult = RefereeResult { refereeLeft  :: Maybe RefereeDecision
                                   , refereeMain  :: Maybe RefereeDecision
                                   , refereeRight :: Maybe RefereeDecision }

instance ToJSON RefereeResult where
  toJSON RefereeResult {..} = toJSON (refereeLeft, refereeMain, refereeRight)

emptyRefereeResult :: RefereeResult
emptyRefereeResult = RefereeResult Nothing Nothing Nothing

instance Show RefereeResult where
  show (RefereeResult l m r) = show [l,m,r]

data Plate = Plate25 | Plate20 | Plate15 | Plate10 | Plate5 | Plate2_5 | Plate1_25 deriving (Show, Enum)

-- (lifterName, lifterClub, currDiscipline, currGroupNr, nextAttemptNr, nextWeight, plates)
type LifterAttemptInfo = (Text, Text, Text, Int, Maybe AttemptNr, Double, [(Plate, Int)])

data FrontendMessage = LifterUpdate (MeetState, [Lifter])
                     | JuryResult (Maybe LifterAttemptInfo, RefereeResult, Bool)
                         -- The bool indicates whether the frontend
                         -- shows the result in an overlay or not at all

resultList :: Results -> [Discipline]
resultList res = (\(_,l) -> res ^. (unpackLens'NT l)) <$> meetType

getHighestAttempt :: [Attempt] -> Maybe Double
getHighestAttempt = maximumMay . catMaybes . map attemptWeight

getBestAttempt :: Discipline -> Maybe Double
getBestAttempt = getHighestAttempt . filter attemptSuccess . attemptsAsList

getDisciplineFromLifter :: Text -> Lifter -> Discipline
getDisciplineFromLifter n Lifter {..} = fromJust $ P.lookup n $ zip disciplineNames (resultList $ lifterRes)
  where
    disciplineNames = fst <$> meetType

emptyMeetState :: MeetState
emptyMeetState =
  MeetState { meetStateCurrDiscipline = fst . P.head $ meetType
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
  toJSON res = toJSON $ (\(n,l) -> (n, res ^. (unpackLens'NT l))) <$> meetType

disciplinesAsList :: Results -> [Discipline]
disciplinesAsList res = (\(_,Lens'NT l) -> res ^. l) <$> meetType
