{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketHelper where

import Import
import Control.Lens ((^.), (%~))
import Scoresheetlogic
import qualified Data.List as L

-- Admin
computeRefereeChan :: (Maybe LifterAttemptInfo, RefereeResult, Bool) -> FrontendMessage
computeRefereeChan (lAttInfo,res,b) = JuryResultMessage
                                        (toJSON ("JuryData" :: Text, lAttInfo, res))
                                        b

getPrognosedPlacing :: MeetState -> ELifter -> [ELifter] -> Placing
getPrognosedPlacing ms (k,l) els =
  fromMaybe 0 $ (\els' -> getPlacingELifter (k,l) els') <$> newEls

  where
    newEls = (:) <$> updateLifterAttempt <*> pure (L.deleteBy (\(k',_) (k'',_) -> k' == k'') (k,l) els)
    updateLifterAttempt :: Maybe ELifter
    updateLifterAttempt = do
      Lens'NT discLens <- L.lookup (meetStateCurrDiscipline ms) meetType
      aNr <- nextAttemptNr ms l
      let attempt = getAttempt aNr $ (lifterRes l) ^. discLens
      ma  <- validateAttemptDummy attempt
      let res = discLens %~ (setDiscipline aNr ma) $ lifterRes l
      Just $ (k, l { lifterRes = res } )

getLivestreamInfo :: MeetState -> ELifter -> [ELifter] -> Value
getLivestreamInfo ms (k,l@Lifter {..}) els = object [ "lifterName" .= lifterName
                                                    , "lifterClub" .= lifterClub
                                                    , "lifterAgeclass" .= show lifterAgeclass
                                                    , "lifterWeightclass" .= show lifterWeightclass
                                                    , "currentDiscipline" .= meetStateCurrDiscipline ms
                                                    , "results" .= lifterRes
                                                    , "sex" .= show lifterSex
                                                    , "total" .= getTotalLifter l
                                                    , "raw" .= lifterRaw
                                                    , "placing" .= getPlacing l (snd <$> els)
                                                    , "progPlacing" .= getPrognosedPlacing ms (k,l) els ]

-- second argument is next lifter
computeLivestreamInfoChan ::  MeetState -> ELifter -> [ELifter] -> FrontendMessage
computeLivestreamInfoChan ms nextLifter lifters  =
    LifterLiveStreamMessage $ toJSON ("LifterInfoData" :: Text, getLivestreamInfo ms nextLifter lifters)

-- meetState, nextLifter, nextWeight
getLifterAttemptInfo :: MeetState -> Lifter -> Double -> LifterAttemptInfo
getLifterAttemptInfo ms l@Lifter {..} w =
  ( lifterName, lifterClub, meetStateCurrDiscipline ms
  , meetStateCurrGroupNr ms, nextAttemptNr ms l, w, getPlates w)

-- meetState, nextLifterInGroup, nextWeight
computeJuryInfoDataChan :: MeetState -> Lifter -> Weight -> FrontendMessage
computeJuryInfoDataChan ms l w = JuryFrontendInfoMessage $ toJSON $ getLifterAttemptInfo ms l w

-- Frontend
computeFrontendDataChan :: (MeetState, [Lifter]) -> FrontendMessage
computeFrontendDataChan (ms, lifters) =
  do
     let groupNr = meetStateCurrGroupNr ms
     let lifterGroupList = filter (\l -> lifterGroup l == groupNr) lifters :: [Lifter]
     let nextLifters = sortBy (cmpLifterOrder ms) lifterGroupList
     let nextLiftersFiltered = filter (\l -> isJust $ nextWeight ms l) nextLifters
     let nextLiftersOutput = map (\l -> (lifterName l, nextWeight ms l, nextAttemptNr ms l))
                                 nextLiftersFiltered
     let mc = getClass <$> safeHead nextLiftersFiltered
     let liftersSortedByClass = case mc of
                                  (Just c) -> sortBy (cmpLifterClassPrio c) lifters
                                  Nothing  -> sortBy cmpLifterClass lifters
     let liftersGroupedByClass = map (sortBy cmpLifterTotalAndBw) $ L.groupBy (\l l' -> getClass l == getClass l') liftersSortedByClass
     let liftersOverview = map (map $ \l -> (isNext (safeHead nextLiftersFiltered) l,l, showTotal l))
                               liftersGroupedByClass:: [[(Bool,Lifter,Text)]]
     -- The Bool indicates if the Lifter is the next
     LifterFrontendMessage $ toJSON ("SheetData" :: Text, (liftersOverview, nextLiftersOutput, meetStateCurrDiscipline ms, fst <$> meetType))
     where
        isNext :: Maybe Lifter -> Lifter -> Bool
        isNext (Just l') l = l == l'
        isNext _ _         = False

computeSteckerDataChan :: MeetState -> Maybe (Lifter, Double) -> Maybe (Lifter, Double) -> FrontendMessage
computeSteckerDataChan ms m1 m2 =
  SteckerInfoMessage $ toJSON $ doubleMap (map $ \(l,w) -> getLifterAttemptInfo ms l w) $ (m1,m2)

doubleMap :: (a -> b) -> (a,a) -> (b,b)
doubleMap f = bimap f f

