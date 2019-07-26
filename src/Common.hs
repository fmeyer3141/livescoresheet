{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Import
import Sex
import Data.CSV.Conduit
import qualified Data.Text as T
import qualified Data.List as L
import Scoresheetlogic

parseCSV :: ConduitT () ByteString (ResourceT IO) () -> IO [Row Text]
parseCSV rawFile =
    runResourceT $ runConduit $
    rawFile .| intoCSV defCSVSettings .| sinkList --defCSVSettings means , seperator and " to enclose fields

showWilks :: Sex -> Maybe Double -> Double -> Text
showWilks _   Nothing      _          = ""
showWilks sex (Just total) bodyweight = pack $ show $ ((flip (/)) 1000 :: Double -> Double) $
                                          fromIntegral $ (round :: Double -> Int) $
                                          (*) 1000 $ ((fromRational $ wilks * tot) :: Double)
  where
    am :: Rational
    am = -216.0475144
    bm :: Rational
    bm = 16.2606339
    cm :: Rational
    cm = -0.002388645
    dm :: Rational
    dm = -0.00113732
    em :: Rational
    em = 7.01863E-06
    fm :: Rational
    fm = -1.291E-08
    af :: Rational
    af = 594.31747775582
    bf :: Rational
    bf = -27.23842536447
    cf :: Rational
    cf = 0.82112226871
    df :: Rational
    df = -0.00930733913
    ef :: Rational
    ef =47.31582E-06
    ff :: Rational
    ff = -9.054E-08
    tot = toRational total
    bw = toRational bodyweight
    wilks = case sex of
              Male -> 500/(am + bm*bw + cm*bw^(2::Int) + dm*bw^(3::Int) + em*bw^(4::Int) + fm*bw^(5::Int))
              Female -> 500/(af + bf*bw + cf*bw^(2::Int) + df*bw^(3::Int) + ef*bw^(4::Int) + ff*bw^(5::Int))

showAttempt :: Attempt -> Text
showAttempt (Attempt (Success w) _) = T.pack $ show w
showAttempt (Attempt (Fail    w) _) = "-" ++ T.pack (show w)
showAttempt _                       = ""

liftersWithPlacings :: [Lifter] -> [[(Int,Lifter)]]
liftersWithPlacings lifters = map (zip [1..]) (liftersGrouped lifters)

liftersGrouped :: [Lifter] -> [[Lifter]]
liftersGrouped lifters = map (L.sortBy cmpLifterPlacing) $
                             L.groupBy (\l1 l2 -> (lifterRaw l1, lifterSex l1, lifterAgeclass l1, lifterWeightclass l1) ==
                        (lifterRaw l2, lifterSex l2, lifterAgeclass l2, lifterWeightclass l2)) liftersSorted
  where
    liftersSorted = L.sortBy (\l1 l2 -> compare (lifterAgeclass l1, lifterSex l1, lifterWeightclass l1, lifterRaw l1)
                             (lifterAgeclass l2, lifterSex l2, lifterWeightclass l2, lifterRaw l2)) lifters
