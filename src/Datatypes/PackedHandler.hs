{-# LANGUAGE NoImplicitPrelude #-}

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

-- Wrapper around Handler to compose atomic actions
newtype PackedHandlerFor site a = Packed (HandlerFor site a)
newtype PackedHandlerLock    = Lock (MVar ())

class HasPackedHandlerLock site where
  getPackedHandlerLock :: site -> PackedHandlerLock

unpack :: PackedHandlerFor site a -> HandlerFor site a
unpack (Packed h) = h

instance Functor (PackedHandlerFor site) where
  fmap f (Packed h)= Packed $ map f h

instance Applicative (PackedHandlerFor site) where
  pure x                        = Packed $ pure x
  (<*>) (Packed af) (Packed ah) = Packed $ af <*> ah

instance Monad (PackedHandlerFor site) where
  return = pure
  (>>=) (Packed h) f = Packed $ h >>= (unpack . f)

instance MonadIO (PackedHandlerFor site) where
  liftIO = Packed . liftIO

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
