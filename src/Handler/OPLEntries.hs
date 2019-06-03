{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.OPLEntries (getOPLEntriesR) where

import Import
import qualified Data.Text as T
import PackedHandler
import ManageScoresheetState
import Scoresheetlogic (getTotalLifter, lifterPending)
import Sex
import Weightclass
import Ageclass
import Handler.Admin (liftersWithPlacings)

showSex :: Sex -> Text
showSex Male   = "M"
showSex Female = "F"

showRaw :: Bool -> Text
showRaw True  = "Raw"
showRaw False = "Single-ply"

showWeightclass :: Weightclass -> Text
showWeightclass (Minusclass w) = T.pack $ show w
showWeightclass (Plusclass  w) = (T.pack $ show w) ++ "+"

showAgeclass :: Ageclass -> Text
showAgeclass Subjunior = "Sub-Juniors"
showAgeclass Junior    = "Juniors"
showAgeclass Open      = "Open"
showAgeclass Master1   = "Masters 1"
showAgeclass Master2   = "Masters 2"
showAgeclass Master3   = "Masters 3"
showAgeclass Master4   = "Masters 4"

showAttempt :: Attempt -> Text
showAttempt (Attempt (Success w) _) = T.pack $ show w
showAttempt (Attempt (Fail    w) _) = "-" ++ T.pack (show w)
showAttempt _                       = ""

showTotal :: Lifter -> Text
showTotal l = case getTotalLifter l of
  Just t  -> T.pack $ show t
  Nothing -> ""

showBestAttempt :: Discipline -> Text
showBestAttempt d = case getBestAttempt d of
  Just a  -> T.pack $ show a
  Nothing -> "-100"

showPlacing :: Lifter -> Int -> Text
showPlacing l@Lifter{..} pl =
  case (lifterOutOfCompetition, getTotalLifter l) of
    (True, _)        -> "G" -- a.K.
    (False, Just _)  -> pack $ show pl
    (False, Nothing) -> "DQ" --DQ

getOPLEntriesR :: Handler TypedContent
getOPLEntriesR = do
  lifters <- filter (not . lifterPending) <$> atomicallyUnpackHandler getLiftersFromDB
  let content = T.intercalate "\n" $ map (T.intercalate "\n") $ map lifterToCSV <$> (liftersWithPlacings lifters)
  let csv = csvHeader ++ content
  pure $ TypedContent "text/csv" $ toContent csv

  where
    csvHeader = "Name,Sex,Equipment,BodyweightKg,WeightClassKg,BirthYear,Division"
            ++ ",Squat1Kg,Squat2Kg,Squat3Kg,Best3SquatKg" ++ "Bench1Kg,Bench2Kg,Bench3Kg,Best3BenchKg"
            ++ ",Deadlift1Kg,Deadlift2Kg,Deadlift3Kg,Best3DeadliftKg"
            ++ ",TotalKg,Place,Event\n"
    lifterToCSV :: (Int,Lifter) -> Text
    lifterToCSV (pl,l@Lifter {..}) =
      let sqDisc    = getDisciplineFromLifter "Squat"    l in
      let benchDisc = getDisciplineFromLifter "Bench"    l in
      let deadDisc  = getDisciplineFromLifter "Deadlift" l in
      T.intercalate ","
        [ lifterName, showSex lifterSex, showRaw lifterRaw, T.pack $ show lifterWeight
        , showWeightclass lifterWeightclass, T.pack $ show lifterAge, showAgeclass lifterAgeclass
        , showAttempt $ att1 sqDisc,showAttempt $ att2 sqDisc,showAttempt $ att3 sqDisc, showBestAttempt sqDisc
        , showAttempt $ att1 benchDisc,showAttempt $ att2 benchDisc,showAttempt $ att3 benchDisc, showBestAttempt benchDisc
        , showAttempt $ att1 deadDisc,showAttempt $ att2 deadDisc,showAttempt $ att3 deadDisc, showBestAttempt deadDisc
        , showTotal l, showPlacing l pl, "SBD"]
