{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Misc where

import Import
import Yesod.WebSockets
import Network.WebSockets (ConnectionException (..))
import Data.Aeson (encode)

import Data.Text as T

import Handler.Admin

connectionError :: ConnectionException -> WebSocketsT Handler ()
connectionError (ParseException s)   = liftIO $ putStrLn $ "ParseException " ++ T.pack s
connectionError (UnicodeException s) = liftIO $ putStrLn $ "UnicodeException " ++ T.pack s
connectionError _                    = pure () -- Connection closed

dataSocket :: ((MeetState, [Lifter]) -> Value) -> WebSocketsT Handler ()
dataSocket computeData = do
  c <- appFrontendChannel <$> getYesod
  dbData <- lift getDataFromDB
  sendJSON dbData
  rChan <- Import.atomically $ dupTChan c
  catch
    (race_
      (forever $ (atomically $ readTChan rChan) >>= sendJSON)
      (receiveData >>= discardRes) ) -- trick to check for connection closure
    connectionError

  where
    sendJSON :: (MeetState, [Lifter]) -> WebSocketsT Handler ()
    sendJSON d = sendTextData (encode $ computeData d)
    discardRes :: ByteString -> WebSocketsT Handler ()
    discardRes _ = pure ()
