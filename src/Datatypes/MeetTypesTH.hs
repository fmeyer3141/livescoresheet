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
data Attempt = Unset UTCTime
             | Todo Weight UTCTime
             | Success Weight UTCTime
             | Fail Weight UTCTime
             | Skip UTCTime deriving (Show, Read, Eq)

instance ToJSON Attempt where
  toJSON (Unset _)     = object  ["statusCode" .= ("unset" :: Text)]
  toJSON (Todo w _)    = object  ["statusCode" .= ("todo" :: Text), "weight" .= show w]
  toJSON (Success w _) = object  ["statusCode" .= ("success" :: Text), "weight" .= show w]
  toJSON (Fail w _)    = object  ["statusCode" .= ("fail" :: Text), "weight" .= show w]
  toJSON (Skip _)      = object  ["statusCode" .= ("skip" :: Text)]
data LiftModifier = MTodo | MGood | MFail | MSkip deriving Eq
data Discipline = Discipline { att1 :: Attempt, att2 :: Attempt, att3 :: Attempt} deriving (Show, Read, Eq)

attemptFromDB :: Int -> Double -> UTCTime -> Attempt
attemptFromDB 0 _ t = Unset t
attemptFromDB 1 w t = Todo w t
attemptFromDB 2 w t = Success w t
attemptFromDB 3 w t = Fail w t
attemptFromDB _ _ t = Skip t

attemptToDB :: Attempt -> (Int, Double, UTCTime)
attemptToDB (Unset t)     = (0, 0.0, t)
attemptToDB (Todo w t)    = (1, w, t)
attemptToDB (Success w t) = (2, w, t)
attemptToDB (Fail w t)    = (3, w, t)
attemptToDB (Skip t)      = (4, 0.0, t)

attGetChangedTime :: Attempt -> UTCTime
attGetChangedTime (Unset t)     = t
attGetChangedTime (Todo _ t)    = t
attGetChangedTime (Success _ t) = t
attGetChangedTime (Fail _ t)    = t
attGetChangedTime (Skip  t)     = t

validateAttempt :: UTCTime -> Attempt -> Maybe Attempt
validateAttempt t (Todo w _) = Just $ Success w t
validateAttempt _ _          = Nothing

validateAttemptDummy :: Attempt -> Maybe Attempt
validateAttemptDummy (Todo w t) = Just $ Success w t
validateAttemptDummy _          = Nothing

inValidateAttempt :: UTCTime -> Attempt -> Maybe Attempt
inValidateAttempt t (Todo w _) = Just $ Fail w t
inValidateAttempt _ _          = Nothing

attSetChangedDate :: UTCTime -> Attempt -> Attempt
attSetChangedDate t' (Unset _)     = Unset t'
attSetChangedDate t' (Todo w _)    = Todo w t'
attSetChangedDate t' (Success w _) = Success w t'
attSetChangedDate t' (Fail w _)    = Fail w t'
attSetChangedDate t' (Skip _)      = Unset t'

attemptFail :: Attempt -> Bool
attemptFail (Fail _ _) = True
attemptFail (Skip _ )  = True
attemptFail _          = False

attemptSuccess :: Attempt -> Bool
attemptSuccess (Success _ _) = True
attemptSuccess _             = False

attemptToModifier :: Attempt -> LiftModifier
attemptToModifier (Unset _)     = MTodo
attemptToModifier (Todo _ _)    = MTodo
attemptToModifier (Success _ _) = MGood
attemptToModifier (Fail _ _)    = MFail
attemptToModifier (Skip _)      = MSkip

attemptWeight :: Attempt -> Maybe Double
attemptWeight (Todo w _)    = Just w
attemptWeight (Success w _) = Just w
attemptWeight (Fail w _)    = Just w
attemptWeight _             = Nothing

attemptPending :: Attempt -> Bool
attemptPending (Unset _)  = True
attemptPending (Todo _ _) = True
attemptPending _          = False

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

emptyAttempt :: UTCTime -> Attempt
emptyAttempt = Unset

emptyDiscipline :: UTCTime -> Discipline
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

setDiscipline :: Int -> Attempt -> Discipline -> Maybe Discipline
setDiscipline 1 att d = Just $ d {att1 = att}
setDiscipline 2 att d = Just $ d {att2 = att}
setDiscipline 3 att d = Just $ d {att3 = att}
setDiscipline _ _   _ = Nothing

markAttempt :: UTCTime -> Bool -> Attempt -> Maybe Attempt
markAttempt t True  = validateAttempt t
markAttempt t False = inValidateAttempt t

getAttempt :: Int -> Discipline -> Maybe Attempt
getAttempt 1 d = Just $ att1 d
getAttempt 2 d = Just $ att2 d
getAttempt 3 d = Just $ att3 d
getAttempt _ _ = Nothing
