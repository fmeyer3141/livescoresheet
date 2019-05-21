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

import qualified Data.Text as T
import qualified Database.Esqueleto as E

import Control.Monad.State (execState)
import Control.Lens ((^.), (%=))
import PackedHandler
import SocketHelper

import Data.Time.Clock.POSIX
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
initialSetupDB lifterList groupNr =
  resetDB
  *> runDB (insertMany $ fromLifterList lifterList)
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
  fromMaybe emptyMeetState . fEntityVal . headMay <$> dbQuery

updateMeetState :: MeetState -> PackedHandler ()
updateMeetState MeetState {..} =
  runDB (updateWhere ([] :: [Filter MeetState])
           [ MeetStateCurrGroupNr =. meetStateCurrGroupNr
           , MeetStateCurrDiscipline =. meetStateCurrDiscipline])
  *> pushDataFromDBToChannel

getDataFromDB :: PackedHandler (MeetState, [(Key Lifter', Lifter)])
getDataFromDB =
  (,) <$> getCurrMeetStateFromDB <*> getELiftersFromDB

updateLiftersInDBWithGroupNr :: GroupNr -> [(Key Lifter', Lifter)] -> PackedHandler ()
updateLiftersInDBWithGroupNr groupNr = updateLiftersInDB . filter ((==) groupNr . lifterGroup . snd)

updateLiftersInDB :: [(Key Lifter', Lifter)] -> PackedHandler ()
updateLiftersInDB args = do -- perform backup
  toBackup <- getLiftersFromDB
  backVersions <- getAllBackupVersionsDesc
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
      time <- realToFrac <$> liftIO getPOSIXTime
      liftersInDB <- getELiftersFromDB
      sequence_ $ (updatePr time liftersInDB) <$> args'

    updatePr :: AttemptTime -> [(Key Lifter', Lifter)] -> (Key Lifter', Lifter) -> PackedHandler ()
    updatePr t els el@(lId, l) = do
      let res = find ((==) lId . fst) els
      case res of
        Just fl -> updateLifterInDB t (fl, l)
        Nothing -> $logError $ "Could not find Lifter " ++ (T.pack $ show el) ++ " in DB"

    -- Entity Lifter consisting of the first two tuple elements
    -- is the old version from the db, Lifter contains the (perhaps) new attributes
    updateLifterInDB :: AttemptTime -> ((Key Lifter', Lifter), Lifter) -> PackedHandler ()
    updateLifterInDB t ((lId, l), l') = do
      runDB $
        updateLifter' lId (lifterWeight l') $ updateLifterRes t (lifterRes l) (lifterRes l')
    -- store/keep the newest entry in the DB
    updateLifterRes :: AttemptTime -> Results -> Results -> Results
    updateLifterRes t res = execState $
      forM_ (snd <$> meetType) $ \(Lens'NT l) ->
        l %= (updateLifterDisc t (res ^. l))

    updateLifterDisc :: AttemptTime -> Discipline -> Discipline -> Discipline
    updateLifterDisc t d d' = Discipline { att1 = processAtt t (att1 d) (att1 d')
                                         , att2 = processAtt t (att2 d) (att2 d')
                                         , att3 = processAtt t (att3 d) (att3 d') }

    processAtt :: AttemptTime -> Attempt -> Attempt -> Attempt
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

getAllBackupVersionsDesc :: PackedHandler [Int]
getAllBackupVersionsDesc =
  do
    bVs <- runDB $ E.select $ E.distinct $ E.from $ \lifterB' -> do
             E.orderBy [E.desc (lifterB' E.^. LifterBackup'Version)]
             return (lifterB' E.^. LifterBackup'Version)
    pure $ E.unValue <$> bVs

-- Ints already sorted descending
getLatestBackupVersionWithVersions :: [Int] -> Maybe Int
getLatestBackupVersionWithVersions = headMay

getLatestBackupVersion :: PackedHandler (Maybe Int)
getLatestBackupVersion = map getLatestBackupVersionWithVersions getAllBackupVersionsDesc

backupLifter :: Int -> [Lifter] -> PackedHandler ()
backupLifter v =
  void . runDB . insertMany . map (\l -> toLifterBackup' $ LifterBackup l v)

truncBackupHistoryWithVersions :: [Int] -> PackedHandler ()
truncBackupHistoryWithVersions backupVersions = do
  maybe (pure ())
    (\deleteBeforeVersion ->
      runDB $ deleteWhere [LifterBackup'Version <. deleteBeforeVersion])
    (backupVersions !! 9)

pushInChannel :: FrontendMessage -> PackedHandler ()
pushInChannel m = packHandler $ do
  wChan <- appFrontendChannel <$> getYesod
  atomically $ writeTChan wChan m

pushDataToChannel :: (MeetState, [(Key Lifter', Lifter)]) -> PackedHandler ()
pushDataToChannel = mapM_ pushInChannel . getFrontendMessagesFromData

getFrontendMessagesFromData :: (MeetState, [(Key Lifter', Lifter)]) -> [FrontendMessage]
getFrontendMessagesFromData (ms, els) =
  let ls              = snd <$> els in
  let next2Lifters    = getNext2LiftersInGroupWithf (snd) ms els in
  let nextELifter     = fst next2Lifters in
  let nextLifter      = snd <$> nextELifter in
  let nWeight         = nextLifter >>= nextWeight ms in
  let nextNextELifter = snd next2Lifters in
  let nextNextLifter  = snd <$> nextNextELifter in
  let nnWeight        = nextNextLifter >>= nextWeight ms in
  let helpF           = liftA2 (,) in

  [computeFrontendDataChan (ms, ls)] ++
  (case nextELifter of
    Just (e,nL)   ->
        [computeLivestreamInfoChan ms (e,nL) els] ++
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
