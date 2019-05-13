{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module MeetTypesTH where

import ClassyPrelude.Yesod
import Language.Haskell.TH
import Settings
import Yesod.Default.Config2
import Data.Text as T

type Weight = Double
-- Store time as Double with getPOSIXTime for better db performance
type AttemptTime = Double
data AttemptStatus = Unset
                   | Todo    Weight
                   | Success Weight
                   | Fail Weight
                   | Skip            deriving (Show, Read, Eq)

data Attempt =  Attempt AttemptStatus AttemptTime deriving (Show, Read, Eq)

data AttemptNr = Attempt1 | Attempt2 | Attempt3 deriving (Eq, Ord)

instance ToJSON AttemptNr where
  toJSON Attempt1 = toJSON (1 :: Int)
  toJSON Attempt2 = toJSON (2 :: Int)
  toJSON Attempt3 = toJSON (3 :: Int)

instance ToJSON Attempt where
  toJSON (Attempt Unset       _) = object  ["statusCode" .= ("unset" :: Text)]
  toJSON (Attempt (Todo w)    _) = object  ["statusCode" .= ("todo" :: Text), "weight" .= show w]
  toJSON (Attempt (Success w) _) = object  ["statusCode" .= ("success" :: Text), "weight" .= show w]
  toJSON (Attempt (Fail w)    _) = object  ["statusCode" .= ("fail" :: Text), "weight" .= show w]
  toJSON (Attempt Skip        _) = object  ["statusCode" .= ("skip" :: Text)]

data LiftModifier = MTodo | MGood | MFail | MSkip deriving Eq
data Discipline = Discipline { att1 :: Attempt, att2 :: Attempt, att3 :: Attempt} deriving (Show, Read, Eq)

attemptFromDB :: Int -> Double -> AttemptTime -> Attempt
attemptFromDB 0 _ t = Attempt Unset       t
attemptFromDB 1 w t = Attempt (Todo w)    t
attemptFromDB 2 w t = Attempt (Success w) t
attemptFromDB 3 w t = Attempt (Fail w)    t
attemptFromDB _ _ t = Attempt Skip        t

attemptToDB :: Attempt -> (Int, Double, AttemptTime)
attemptToDB (Attempt Unset      t)  = (0, 0.0, t)
attemptToDB (Attempt (Todo w)   t)  = (1, w, t)
attemptToDB (Attempt (Success w) t) = (2, w, t)
attemptToDB (Attempt (Fail w) t)    = (3, w, t)
attemptToDB (Attempt Skip t)        = (4, 0.0, t)

attGetChangedTime :: Attempt -> AttemptTime
attGetChangedTime (Attempt _ t) = t

validateAttempt :: AttemptTime -> Attempt -> Maybe Attempt
validateAttempt t (Attempt (Todo w) _) = Just $ Attempt (Success w) t
validateAttempt _ _                    = Nothing

validateAttemptDummy :: Attempt -> Maybe Attempt
validateAttemptDummy (Attempt (Todo w) t) = Just $ Attempt (Success w) t
validateAttemptDummy _                    = Nothing

inValidateAttempt :: AttemptTime -> Attempt -> Maybe Attempt
inValidateAttempt t (Attempt (Todo w) _) = Just $ Attempt (Fail w) t
inValidateAttempt _ _                    = Nothing

attSetChangedDate :: AttemptTime -> Attempt -> Attempt
attSetChangedDate t' (Attempt s _)     = Attempt s t'

attemptFail :: Attempt -> Bool
attemptFail (Attempt (Fail _) _) = True
attemptFail (Attempt Skip     _) = True
attemptFail _                    = False

attemptSuccess :: Attempt -> Bool
attemptSuccess (Attempt (Success _) _) = True
attemptSuccess _                       = False

attemptToModifier :: Attempt -> LiftModifier
attemptToModifier (Attempt Unset    _)    = MTodo
attemptToModifier (Attempt (Todo    _) _) = MTodo
attemptToModifier (Attempt (Success _) _) = MGood
attemptToModifier (Attempt (Fail _) _)    = MFail
attemptToModifier (Attempt Skip     _)    = MSkip

attemptWeight :: Attempt -> Maybe Double
attemptWeight (Attempt (Todo w)    _) = Just w
attemptWeight (Attempt (Success w) _) = Just w
attemptWeight (Attempt (Fail w)    _) = Just w
attemptWeight _                       = Nothing

attemptPending :: Attempt -> Bool
attemptPending (Attempt Unset    _) = True
attemptPending (Attempt (Todo _) _) = True
attemptPending _                    = False

readMeetSettings :: FilePath -> IO MeetSettings
readMeetSettings fp = loadYamlSettings [fp] [] useEnv

meetSettingsFile :: FilePath
meetSettingsFile = "config/meetSettings.yml"

meetSettings :: IO MeetSettings
meetSettings = readMeetSettings meetSettingsFile

instance ToJSON Discipline where
  toJSON Discipline {..} = object [ "att1" .= toJSON att1, "att2" .= toJSON att2, "att3" .= toJSON att3]

attemptsAsList :: Discipline -> [Attempt]
attemptsAsList Discipline {..} = [att1,att2,att3]

emptyAttempt :: AttemptTime -> Attempt
emptyAttempt = Attempt $ Unset

emptyDiscipline :: AttemptTime -> Discipline
emptyDiscipline =  Discipline <$> emptyAttempt <*> emptyAttempt <*> emptyAttempt

readDisciplines :: IO [Text]
readDisciplines = meetTypeStr <$> meetSettings

resultsTypeTH :: Q [Dec]
resultsTypeTH = do
    disc <- liftIO readDisciplines
    (pure . pure) $ DataD [] typeName [] Nothing [constr disc] []

    where
      typeName = mkName "Results"
      constr disc = RecC typeName [(mkName ("_disc" ++ T.unpack name), defBang, attType) | name<-disc]
      defBang = Bang NoSourceUnpackedness NoSourceStrictness
      attType = ConT ''Discipline

setDiscipline :: AttemptNr -> Attempt -> Discipline -> Discipline
setDiscipline Attempt1 att d = d {att1 = att}
setDiscipline Attempt2 att d = d {att2 = att}
setDiscipline Attempt3 att d = d {att3 = att}

markAttempt :: AttemptTime -> Bool -> Attempt -> Maybe Attempt
markAttempt t True  = validateAttempt t
markAttempt t False = inValidateAttempt t

getAttempt :: AttemptNr -> Discipline -> Attempt
getAttempt Attempt1 d = att1 d
getAttempt Attempt2 d = att2 d
getAttempt Attempt3 d = att3 d
