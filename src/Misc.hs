{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Misc where

import Import
import Ageclass
import Weightclass
import Yesod.WebSockets
import Network.WebSockets (ConnectionException (..))
import Data.Aeson (encode)

import Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as Maybe (fromJust)
import Control.Lens ((%%~), (^.))

import ManageScoresheetState
import Scoresheetlogic
import PackedHandler

import Control.Monad.Logger

getPrognosedPlacing :: MeetState -> Lifter -> [Lifter] -> Placing
getPrognosedPlacing ms l ls =
  fromMaybe 0 $ (\l' -> getPlacing ms l' ls) <$> updateLifterAttempt ms l ls
  where
    updateLifterAttempt :: MeetState -> Lifter -> [Lifter] -> Maybe Lifter
    updateLifterAttempt ms l ls = do
      Lens'NT discLens <- L.lookup (meetStateCurrDiscipline ms) meetType
      aNr <- nextAttemptNr ms l
      attempt <- getAttempt aNr $ (lifterRes l) ^. discLens
      ma  <- markAttempt undefined True attempt
      res <- discLens %%~ (setDiscipline aNr ma) $ lifterRes l
      Just $ l { lifterRes = res }


getLifterAttemptInfo :: MeetState -> Lifter -> Maybe LifterAttemptInfo
getLifterAttemptInfo ms l@Lifter {..} =
  do
    w             <- nextWeight ms l
    return ( lifterName, lifterClub, meetStateCurrDiscipline ms
           , meetStateCurrGroupNr ms, nextAttemptNr ms l, w, getPlates w)

getLivestreamInfo :: MeetState -> Lifter -> [Lifter] -> Maybe Value
-- TODO
getLivestreamInfo ms l@Lifter {..} ls = Just $ object [ "lifterName" .= lifterName
                                                      , "lifterClub" .= lifterClub
                                                      , "lifterAgeclass" .= show lifterAgeclass
                                                      , "lifterWeightclass" .= show lifterWeightclass
                                                      , "currentDiscipline" .= meetStateCurrDiscipline ms
                                                      , "results" .= lifterRes
                                                      , "sex" .= show lifterSex
                                                      , "total" .= getTotalLifter l
                                                      , "raw" .= lifterRaw
                                                      , "placing" .= getPlacing ms l ls
                                                      , "progPlacing" .= (1 :: Int)]

doubleMap :: (a -> b) -> (a,a) -> (b,b)
doubleMap f = bimap f f

connectionError :: ConnectionException -> WebSocketsT Handler ()
connectionError (ParseException s)   = $logError $ "ParseException " ++ T.pack s
connectionError (UnicodeException s) = $logError $ "UnicodeException " ++ T.pack s
connectionError _                    = logInfoN "websocket closed" -- Connection closed

dataSocket :: (FrontendMessage -> Maybe Value) -> WebSocketsT Handler ()
dataSocket computeData = do
  c <- appFrontendChannel <$> getYesod
  rChan <- Import.atomically $ dupTChan c
  -- send current state for Frontend
  dbData <- lift $ atomicallyUnpackHandler getDataFromDB
  juryStateRef <- appRefereeState <$> getYesod
  refState <- lift . atomicallyUnpackHandler . packHandler $ atomicModifyIORef' juryStateRef $ \a -> (a,a)
  sendJSON $ LifterUpdate dbData
  sendJSON $ JuryResult (Nothing, refState, False)
  catch
    (race_
      (forever $ (atomically $ readTChan rChan) >>= sendJSON)
      (receiveData >>= discardRes) ) -- trick to check for connection closure
    connectionError

  where
    sendJSON :: FrontendMessage -> WebSocketsT Handler ()
    sendJSON d =
      case computeData d of
        Nothing     -> pure ()
        Just toSend -> sendTextData (encode toSend)
    discardRes :: ByteString -> WebSocketsT Handler ()
    discardRes _ = pure ()
