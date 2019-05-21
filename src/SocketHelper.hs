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
  fromMaybe (Just 0) $ (\els' -> getPlacingELifter (k,l) els') <$> newEls

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
     let nextLifters           = getNextLifters ms lifters
     let nextLiftersOutput     = map (\l -> (lifterName l, nextWeight ms l, nextAttemptNr ms l)) $
                                  getNextLifters ms lifters
     let mc                    = getClass <$> headMay nextLifters

     let liftersSortedByClass  = maybe (sortBy cmpLifterClass)
                                      (sortBy . cmpLifterClassPrio) mc $ lifters

     let liftersGroupedByClass = sortBy cmpLifterPlacing <$> L.groupBy ((==) `on` getClass) liftersSortedByClass
     let showLifter l          = (maybe False (== l) $ headMay nextLifters,l, showTotal l)
     let liftersOverview       = map showLifter <$> liftersGroupedByClass:: [[(Bool,Lifter,Text)]]
     -- The Bool indicates if the Lifter is the next
     LifterFrontendMessage $ toJSON ("SheetData" :: Text, (liftersOverview, nextLiftersOutput, meetStateCurrDiscipline ms, fst <$> meetType))

computeSteckerDataChan :: MeetState -> Maybe (Lifter, Double) -> Maybe (Lifter, Double) -> FrontendMessage
computeSteckerDataChan ms m1 m2 =
  SteckerInfoMessage $ toJSON $ doubleMap (map $ \(l,w) -> getLifterAttemptInfo ms l w) $ (m1,m2)

doubleMap :: (a -> b) -> (a,a) -> (b,b)
doubleMap f = bimap f f

