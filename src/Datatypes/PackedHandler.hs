{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PackedHandler ( PackedHandlerFor
                     , pack
                     , runDB
                     , packHandler
                     , PackedHandlerLock
                     , HasPackedHandlerLock
                     , getPackedHandlerLock
                     , getYesodPacked
                     , newPackHandlerLock
                     , atomicallyUnpackHandler) where

import ClassyPrelude.Yesod hiding (runDB, unpack)
import qualified ClassyPrelude.Yesod as Y (runDB)

-- Wrapper around Handler to not forget to compose atomic actions
newtype PackedHandlerFor site a = Packed (HandlerFor site a) deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)
newtype PackedHandlerLock    = Lock (MVar ())

class HasPackedHandlerLock site where
  getPackedHandlerLock :: site -> PackedHandlerLock

unpack :: PackedHandlerFor site a -> HandlerFor site a
unpack (Packed h) = h

runDB :: (YesodPersist site) => YesodDB site a -> PackedHandlerFor site a
runDB = Packed . Y.runDB

packHandler :: HandlerFor site a -> PackedHandlerFor site a
packHandler = Packed

getYesodPacked :: PackedHandlerFor site (HandlerSite (HandlerFor site))
getYesodPacked = Packed getYesod

newPackHandlerLock :: IO PackedHandlerLock
newPackHandlerLock = Lock <$> newMVar ()

atomicallyUnpackHandler :: HasPackedHandlerLock site => PackedHandlerFor site a -> HandlerFor site a
atomicallyUnpackHandler (Packed h) = do
  Lock lock <- getPackedHandlerLock <$> getYesod
  withMVar lock $ \_ -> h
