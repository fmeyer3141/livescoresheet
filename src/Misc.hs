{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Misc where

import Import
import Yesod.WebSockets
import Network.WebSockets (ConnectionException (..))
import Data.Aeson (encode)

import Data.Text as T

import ManageScoresheetState
import PackedHandler

doubleMap :: (a -> b) -> (a,a) -> (b,b)
doubleMap f = bimap f f

connectionError :: ConnectionException -> WebSocketsT Handler ()
connectionError (ParseException s)   = liftIO $ putStrLn $ "ParseException " ++ T.pack s
connectionError (UnicodeException s) = liftIO $ putStrLn $ "UnicodeException " ++ T.pack s
connectionError _                    = pure () -- Connection closed

dataSocket :: (FrontendMessage -> Maybe Value) -> WebSocketsT Handler ()
dataSocket computeData = do
  c <- appFrontendChannel <$> getYesod
  rChan <- Import.atomically $ dupTChan c
  -- send current state for Frontend
  dbData <- lift $ atomicallyUnpackHandler getDataFromDB
  sendJSON $ LifterUpdate dbData
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
