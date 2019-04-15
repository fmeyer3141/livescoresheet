{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-} {-# LANGUAGE RecordWildCards #-}

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
data LiftModifier = MTodo | MGood | MFail | MSkip deriving Eq
data Discipline = Discipline { att1 :: Attempt, att2 :: Attempt, att3 :: Attempt} deriving (Show, Read, Eq)

attGetChangedTime :: Attempt -> UTCTime
attGetChangedTime (Unset t)     = t
attGetChangedTime (Todo _ t)    = t
attGetChangedTime (Success _ t) = t
attGetChangedTime (Fail _ t)    = t
attGetChangedTime (Skip  t)     = t

attSetChangedDate :: UTCTime -> Attempt -> Attempt
attSetChangedDate t' (Unset _)     = Unset t'
attSetChangedDate t' (Todo _ _)    = Unset t'
attSetChangedDate t' (Success _ _) = Unset t'
attSetChangedDate t' (Fail _ _)    = Unset t'
attSetChangedDate t' (Skip _)      = Unset t'

attemptFail :: Attempt -> Bool
attemptFail (Fail _ _) = True
attemptFail _          = True

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

instance ToJSON Attempt where
  toJSON = toJSON . show

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
