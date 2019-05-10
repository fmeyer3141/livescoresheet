{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module ManageScoresheetState ( getDataFromDB
                             , PackedHandler
                             , getELiftersFromDB
                             , getELiftersInGroupFromDB
                             , getCurrMeetStateFromDB
                             , fEntityVal
                             , pushDataFromDBToChannel
                             , getFrontendMessagesFromData
                             , pushRefereeStateToChannel
                             , getLiftersFromDB
                             , getGroupNrsFromDB
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
import qualified Database.Esqueleto as E

import Control.Lens
import PackedHandler
import SocketHelper

import Scoresheetlogic

type PackedHandler a = PackedHandlerFor App a

resetDB :: PackedHandler ()
resetDB = do
  runDB $ deleteWhere ([] :: [Filter Lifter'])
  runDB $ deleteWhere ([] :: [Filter LifterBackup'])
  runDB $ deleteWhere ([] :: [Filter MeetState])

getGroupNrsFromDB :: PackedHandler [GroupNr]
getGroupNrsFromDB =
  do
    gNrs <- runDB $ E.select $ E.distinct $ E.from $ \lifter' -> do
              E.orderBy [E.asc (lifter' E.^. Lifter'Group)]
              return (lifter' E.^. Lifter'Group)
    pure $ E.unValue <$> gNrs

initialSetupDB :: [Lifter] -> GroupNr -> PackedHandler ()
initialSetupDB lifterList groupNr = void $
  runDB (insertMany $ fromLifterList lifterList)
  *> (runDB $ insert $ emptyMeetState { meetStateCurrGroupNr = groupNr })
  *> pushDataFromDBToChannel

getELiftersFromDB :: PackedHandler [(Key Lifter', Lifter)]
getELiftersFromDB = do
    fromDB <- runDB $ selectList ([] :: [Filter Lifter']) ([] :: [SelectOpt Lifter'])
    pure $ map (\(Entity k l) -> (k, toLifter l)) fromDB

getELiftersInGroupFromDB :: GroupNr -> PackedHandler [(Key Lifter', Lifter)]
getELiftersInGroupFromDB gNr = do
    fromDB <- runDB $ selectList [Lifter'Group ==. gNr] ([] :: [SelectOpt Lifter'])
    pure $ map (\(Entity k l) -> (k, toLifter l)) fromDB

fEntityVal :: Functor f => f (Entity a) -> f a
fEntityVal= map entityVal

getLiftersFromDB :: PackedHandler [Lifter]
getLiftersFromDB = (map snd) <$> getELiftersFromDB

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

updateLiftersInDBWithGroupNr :: GroupNr -> [(Key Lifter', Lifter)] -> PackedHandler ()
updateLiftersInDBWithGroupNr groupNr = updateLiftersInDB . filter ((==) groupNr . lifterGroup . snd)

updateLiftersInDB :: [(Key Lifter', Lifter)] -> PackedHandler ()
updateLiftersInDB args = do -- perform backup
  toBackup <- getLiftersFromDB
  backVersions <- getAllBackupVersions
  let latestBackVersion = getLatestBackupVersionWithVersions backVersions
  case latestBackVersion of
    Nothing -> backupLifter 0 toBackup
    Just x -> backupLifter (x+1) toBackup
  updateLiftersInDB' args
  truncBackupHistoryWithVersions backVersions
  -- update Frontends from DB in order to send consistent information
  -- (e.g. were the attempts up-to-date and therefore stored?)
  pushDataFromDBToChannel

  where
    updateLiftersInDB' :: [(Key Lifter', Lifter)] -> PackedHandler ()
    updateLiftersInDB' args' = do
      time <- liftIO getCurrentTime
      liftersInDB <- getELiftersFromDB
      sequence_ $ (updatePr time liftersInDB) <$> args'

    updatePr :: UTCTime -> [(Key Lifter', Lifter)] -> (Key Lifter', Lifter) -> PackedHandler ()
    updatePr t els el@(lId, l) = do
      let res = find ((==) lId . fst) els
      case res of
        Just fl -> updateLifterInDB t (fl, l)
        Nothing -> $logError $ "Could not find Lifter " ++ (T.pack $ show el) ++ " in DB"

    -- Entity Lifter consisting of the first two tuple elements
    -- is the old version from the db, Lifter contains the (perhaps) new attributes
    updateLifterInDB :: UTCTime -> ((Key Lifter', Lifter), Lifter) -> PackedHandler ()
    updateLifterInDB t ((lId, l), l') = do
      runDB $
        updateLifter' lId (lifterGroup l') $ updateLifterRes t (lifterRes l) (lifterRes l')
    -- store/keep the newest entry in the DB
    updateLifterRes :: UTCTime -> Results -> Results -> Results
    updateLifterRes t res res' =
      F.foldl' (\r' (v,m) -> m (updateLifterDisc t (v res)) r') res' $ zip viewLens modifyLens
    updateLifterDisc :: UTCTime -> Discipline -> Discipline -> Discipline
    updateLifterDisc t d d' = Discipline { att1 = processAtt t (att1 d) (att1 d')
                                         , att2 = processAtt t (att2 d) (att2 d')
                                         , att3 = processAtt t (att3 d) (att3 d') }

    processAtt :: UTCTime -> Attempt -> Attempt -> Attempt
    processAtt t a a' =
      let (b,newer) = keepNewer a a' in
      if b then attSetChangedDate t newer else newer

-- The bool indicates if we want to store a new time in the database
    keepNewer :: Attempt -> Attempt -> (Bool, Attempt)
    keepNewer att att' =
      case compare (attGetChangedTime att') (attGetChangedTime att) of
        LT -> (False, att)
        EQ -> (False, att')
        GT -> (True,  att')
    modifyLens = (over . unpackLens'NT . snd) <$> meetType
    viewLens = (view . unpackLens'NT . snd) <$> meetType

getAllBackupVersions :: PackedHandler [Int]
getAllBackupVersions =
  do
    bVs <- runDB $ E.select $ E.distinct $ E.from $ \lifterB' -> do
             E.orderBy [E.desc (lifterB' E.^. LifterBackup'Version)]
             return (lifterB' E.^. LifterBackup'Version)
    pure $ E.unValue <$> bVs

-- Ints already sorted descending
getLatestBackupVersionWithVersions :: [Int] -> Maybe Int
getLatestBackupVersionWithVersions = safeHead

getLatestBackupVersion :: PackedHandler (Maybe Int)
getLatestBackupVersion = map getLatestBackupVersionWithVersions getAllBackupVersions

backupLifter :: Int -> [Lifter] -> PackedHandler ()
backupLifter v =
  void . runDB . insertMany . map (\l -> toLifterBackup' $ LifterBackup l v)

truncBackupHistoryWithVersions :: [Int] -> PackedHandler ()
truncBackupHistoryWithVersions backupVersions = do
  when (length backupVersions >= 10)
    (do
      let deleteAfterVersion =  backupVersions P.!! 9
      runDB $ deleteWhere [LifterBackup'Version >. deleteAfterVersion])

pushInChannel :: FrontendMessage -> PackedHandler ()
pushInChannel m = packHandler $ do
  wChan <- appFrontendChannel <$> getYesod
  atomically $ writeTChan wChan m


pushDataToChannel :: (MeetState, [Lifter]) -> PackedHandler ()
pushDataToChannel = mapM_ pushInChannel . getFrontendMessagesFromData

getFrontendMessagesFromData :: (MeetState, [Lifter]) -> [FrontendMessage]
getFrontendMessagesFromData (ms, ls) =
  let next2Lifters = getNext2LiftersInGroup ms ls in
  let nextLifter = fst next2Lifters in
  let nWeight = nextLifter >>= nextWeight ms in
  let nextNextLifter = snd next2Lifters in
  let nnWeight = nextNextLifter >>= nextWeight ms in
  let helpF = liftA2 (,) in

  [computeFrontendDataChan (ms, ls)] ++
  (case nextLifter of
    Just nL   ->
        [computeLivestreamInfoChan ms nL ls] ++
        (case nWeight of
          Just nW   -> [computeJuryInfoDataChan ms nL nW]
          _         -> [] )
    _               -> [] )
  ++ [computeSteckerDataChan ms (helpF nextLifter nWeight) (helpF nextNextLifter nnWeight)]

pushDataFromDBToChannel :: PackedHandler ()
pushDataFromDBToChannel = getDataFromDB >>= pushDataToChannel

pushRefereeStateToChannel :: (Maybe LifterAttemptInfo, RefereeResult, Bool) -> PackedHandler ()
pushRefereeStateToChannel = pushInChannel . computeRefereeChan

restoreBackup :: PackedHandler ()
restoreBackup = do
  version <- getLatestBackupVersion
  case version of
    Nothing -> pure ()
    Just v ->
      runDB (deleteWhere ([] :: [Filter Lifter'])) -- truncate table
      -- restore
      *> insertVersion v
      *> runDB (deleteWhere [LifterBackup'Version ==. v])
      -- restore
      *> pushDataFromDBToChannel
  where
    insertVersion :: Int -> PackedHandler ()
    insertVersion version = do
      -- get BackupLifters
      backupLifters <- runDB $
        E.select $ E.from $ \lifterBackup' -> do
          E.where_ (lifterBackup' E.^. LifterBackup'Version E.==. E.val version)
          return $ lifterBackup'
      runDB $ E.insertMany_ $ ((toLifter' . lifterBackupLifter . toLifterBackup . entityVal) <$> backupLifters)
