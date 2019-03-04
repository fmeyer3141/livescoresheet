{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module MeetTypesTH where

import ClassyPrelude.Yesod
import Language.Haskell.TH
import Settings
import Yesod.Default.Config2
import Data.Text as T

data Attempt = Attempt { weight :: Maybe Double, success :: Maybe Bool } deriving (Show, Read, Eq)
data Discipline = Discipline { att1 :: Attempt, att2 :: Attempt, att3 :: Attempt} deriving (Show, Read, Eq)

readMeetSettings :: FilePath -> IO MeetSettings
readMeetSettings fp = loadYamlSettings [fp] [] useEnv

meetSettingsFile :: FilePath
meetSettingsFile = "config/meetSettings.yml"

meetSettings :: IO MeetSettings
meetSettings = readMeetSettings meetSettingsFile

instance ToJSON Attempt where
  toJSON Attempt {..} = object [ "weight" .= show weight, "success" .= show success]

instance ToJSON Discipline where
  toJSON Discipline {..} = object [ "att1" .= toJSON att1, "att2" .= toJSON att2, "att3" .= toJSON att3]

attemptsAsList :: Discipline -> [Attempt]
attemptsAsList Discipline {..} = [att1,att2,att3]

resultsTypeTH :: Q [Dec]
resultsTypeTH = do
    disc <- liftIO $ (meetTypeStr <$> meetSettings)
    (pure . pure) $ DataD [] typeName [] Nothing [constr disc] []

    where
      typeName = mkName "Results"
      constr disc = RecC typeName [(mkName ("disc" ++ T.unpack name), defBang, attType) | name<-disc]
      defBang = Bang NoSourceUnpackedness NoSourceStrictness
      attType = ConT ''Discipline