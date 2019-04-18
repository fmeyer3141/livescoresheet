{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ManageScoresheetState ( getDataFromDB
                             , PackedHandler
                             , getELiftersFromDB
                             , getCurrMeetStateFromDB
                             , fEntityVal
                             , pushDataFromDBToChannel
                             , pushRefereeStateToChannel
                             , getLiftersFromDB
                             , resetDB
                             , updateLiftersInDB
                             , updateLiftersInDBWithGroupNr
                             , restoreBackup
                             , initialSetupDB
                             , updateMeetState) where

import Import

import qualified Prelude as P
import qualified Data.Foldable as F
import qualified Data.Text as T

import Control.Lens
import PackedHandler

type PackedHandler a = PackedHandlerFor App a

resetDB :: PackedHandler ()
resetDB = do
  runDB $ deleteWhere ([] :: [Filter Lifter])
  runDB $ deleteWhere ([] :: [Filter LifterBackup])
  runDB $ deleteWhere ([] :: [Filter MeetState])

initialSetupDB :: [Lifter] -> GroupNr -> PackedHandler ()
initialSetupDB lifterList groupNr = void $
  runDB (insertMany lifterList)
  *> (runDB $ insert $ emptyMeetState { meetStateCurrGroupNr = groupNr })
  *> pushDataFromDBToChannel

getELiftersFromDB :: PackedHandler [Entity Lifter]
getELiftersFromDB = runDB $ selectList ([] :: [Filter Lifter]) ([] :: [SelectOpt Lifter])

fEntityVal :: Functor f => f (Entity a) -> f a
fEntityVal= map entityVal

getLiftersFromDB :: PackedHandler [Lifter]
getLiftersFromDB = fEntityVal <$> getELiftersFromDB

getEBackupLiftersFromDB :: PackedHandler [Entity LifterBackup]
getEBackupLiftersFromDB = runDB $ selectList ([] :: [Filter LifterBackup]) []

getBackupLiftersFromDB :: PackedHandler [LifterBackup]
getBackupLiftersFromDB = fEntityVal <$> getEBackupLiftersFromDB

getCurrMeetStateFromDB :: PackedHandler MeetState
getCurrMeetStateFromDB =
  let dbQuery =  runDB $ selectList ([] :: [Filter MeetState]) [] in
  fromMaybe emptyMeetState . fEntityVal . safeHead <$> dbQuery

updateMeetState :: MeetState -> PackedHandler ()
updateMeetState MeetState {..} =
  runDB (updateWhere ([] :: [Filter MeetState])
           [ MeetStateCurrGroupNr =. meetStateCurrGroupNr
           , MeetStateCurrDiscipline =. meetStateCurrDiscipline])
  *> pushDataFromDBToChannel

getDataFromDB :: PackedHandler (MeetState, [Lifter])
getDataFromDB =
  (,) <$> getCurrMeetStateFromDB <*> getLiftersFromDB

updateLiftersInDBWithGroupNr :: GroupNr -> [Entity Lifter] -> PackedHandler ()
updateLiftersInDBWithGroupNr groupNr = updateLiftersInDB . filter ((==) groupNr . lifterGroup . entityVal)

updateLiftersInDB :: [Entity Lifter] -> PackedHandler ()
updateLiftersInDB args = do -- perform backup
  toBackup <- getLiftersFromDB
  backVersion <- getLatestBackupVersion <$> getBackupLiftersFromDB
  case backVersion of
    Nothing -> backupLifter 0 toBackup
    Just x -> backupLifter (x+1) toBackup
  updateLiftersInDB' args
  truncBackupHistory
  -- update Frontends from DB in order to send consistent information
  -- (e.g. were the attempts up-to-date and therefore stored?)
  pushDataFromDBToChannel

  where
    updateLiftersInDB' :: [Entity Lifter] -> PackedHandler ()
    updateLiftersInDB' args' = do
      time <- liftIO getCurrentTime
      liftersInDB <- getELiftersFromDB
      sequence_ $ (updatePr time liftersInDB) <$> args'

    updatePr :: UTCTime -> [Entity Lifter] -> Entity Lifter -> PackedHandler ()
    updatePr t els el@(Entity lId l) = do
      let res = find ((==) lId . entityKey) els
      case res of
        Just fl -> updateLifterInDB t (fl, l)
        Nothing -> liftIO $ putStrLn $ "Could not find Lifter " ++ (T.pack $ show el) ++ " in DB"

    -- Entity Lifter is the old version from the db, Lifter contains the (perhaps) new attributes
    updateLifterInDB :: UTCTime -> (Entity Lifter, Lifter) -> PackedHandler ()
    updateLifterInDB t ((Entity lId l), l') = do
      runDB $
        updateWhere [LifterId ==. lId]
                    [LifterGroup =. lifterGroup l', LifterRes =. updateLifterRes t (lifterRes l) (lifterRes l')]
    -- store/keep the newest entry in the DB
    updateLifterRes :: UTCTime -> Results -> Results -> Results
    updateLifterRes t res res' =
      F.foldl' (\r' (v,m) -> m (updateLifterDisc t (v res)) r') res' $ zip viewLens modifyLens
    updateLifterDisc :: UTCTime -> Discipline -> Discipline -> Discipline
    updateLifterDisc t d d' = Discipline { att1 = attSetChangedDate t $ keepNewer (att1 d) (att1 d')
                                         , att2 = attSetChangedDate t $ keepNewer (att2 d) (att2 d')
                                         , att3 = attSetChangedDate t $ keepNewer (att3 d) (att3 d') }
    keepNewer :: Attempt -> Attempt -> Attempt
    keepNewer att att' =
      case compare (attGetChangedTime att') (attGetChangedTime att) of
        LT -> att
        _  -> att'
    modifyLens = (over . snd) <$> meetType
    viewLens = (view . snd) <$> meetType


getLatestBackupVersion :: [LifterBackup] -> Maybe Int
getLatestBackupVersion = safeHead . sortBy (flip compare) . map lifterBackupVersion

backupLifter :: Int -> [Lifter] -> PackedHandler ()
backupLifter v =
  void . runDB . insertMany . map (LifterBackup v)

truncBackupHistory :: PackedHandler ()
truncBackupHistory = do
  backupDB <- runDB $ selectList [] [Desc LifterBackupVersion]
  let backupVersions = group $ map (lifterBackupVersion . entityVal) backupDB
  when (length backupVersions >= 10)
    (do
      let deleteAfterVersion = P.head $ backupVersions P.!! 9
      runDB $ deleteWhere [LifterBackupVersion >. deleteAfterVersion])

pushInChannel :: FrontendMessage -> PackedHandler ()
pushInChannel m = packHandler $ do
  wChan <- appFrontendChannel <$> getYesod
  atomically $ writeTChan wChan m

pushDataToChannel :: (MeetState, [Lifter]) -> PackedHandler ()
pushDataToChannel = pushInChannel . LifterUpdate

pushDataFromDBToChannel :: PackedHandler ()
pushDataFromDBToChannel = getDataFromDB >>= pushDataToChannel

pushRefereeStateToChannel :: (RefereeResult, Bool) -> PackedHandler ()
pushRefereeStateToChannel = pushInChannel . JuryResult

restoreBackup :: PackedHandler ()
restoreBackup = do
  backups <- getBackupLiftersFromDB
  let version = getLatestBackupVersion backups
  case version of
    Nothing -> pure ()
    Just v ->
      let latestBackup = filter ((==) v . lifterBackupVersion) backups in
      runDB (deleteWhere ([] :: [Filter Lifter])) -- truncate table
      *> runDB (deleteWhere [LifterBackupVersion ==. v])
      -- restore
      *> (runDB . insertMany $ map lifterBackupLifter latestBackup)
      *> pushDataFromDBToChannel
