{-# Language NoImplicitPrelude #-}

module Scoresheetlogic where

import Import
import qualified Prelude as P
import qualified Data.List as L

getTotalLifter :: Lifter -> Maybe Double 
getTotalLifter lifter = getHighestLift (lifterAttemptDL1Weight lifter) (lifterAttemptDL1Success lifter)
                             (lifterAttemptDL2Weight lifter) (lifterAttemptDL2Success lifter)
                             (lifterAttemptDL3Weight lifter) (lifterAttemptDL3Success lifter) 
    where
        getHighestLift a1 s1 a2 s2 a3 s3 = L.minimumBy (P.flip P.compare) 
                                          [getLiftWeight a1 s1,getLiftWeight a2 s2, getLiftWeight a3 s3]
        
        getLiftWeight :: Maybe Double -> Maybe Bool -> Maybe Double
        getLiftWeight (Just x) (Just True) = Just x
        getLiftWeight _ _ = Nothing

nextAttemptNr :: Lifter -> Int
nextAttemptNr l 
            | (lifterAttemptDL1Success l == Nothing) = 1
            | (lifterAttemptDL2Success l == Nothing) = 2
            | (lifterAttemptDL3Success l == Nothing) = 3
            | otherwise = 4

nextWeight:: Lifter -> Int -> Double 
nextWeight l att 
            | att == 1 = n2d (lifterAttemptDL1Weight l)
            | att == 2 = n2d (lifterAttemptDL2Weight l)
            | att == 3 = n2d (lifterAttemptDL3Weight l)
            | otherwise = 1000000 
            where 
              n2d :: Maybe Double -> Double
              n2d (Just d) = d 
              n2d _ = 1000000 

cmpLifterGroupAndTotal :: Int -> Lifter -> Lifter -> Ordering
cmpLifterGroupAndTotal g l1 l2 |((lifterGroup l1) == g) = LT
                               |((lifterGroup l2) == g) = GT
                               |(lifterGroup l1 /= lifterGroup l2) = compare (lifterGroup l1) (lifterGroup l2)
                               |((lifterGroup l1 == lifterGroup l2) && (getTotalLifter l1 /= getTotalLifter l2))
                                 = (flip compare) (getTotalLifter l1) (getTotalLifter l2)
                               |((lifterGroup l1 == lifterGroup l2) && (getTotalLifter l1 == getTotalLifter l2))
                                 = compare (lifterWeight l1) (lifterWeight l2)
cmpLifterGroupAndTotal _ _ _ = EQ

cmpLifterOrder :: Lifter -> Lifter -> Ordering
cmpLifterOrder l1 l2
             | (nextAttemptNr l1) /= (nextAttemptNr l2) = compare (nextAttemptNr l1) (nextAttemptNr l2)
             | (nextWeight l1 $ nextAttemptNr l1) /= (nextWeight l2 $ nextAttemptNr l2) = compare (nextWeight l1 $ nextAttemptNr l1) 
                                                            (nextWeight l2 $ nextAttemptNr l2)
             | (lifterWeight l1) /= (lifterWeight l2) = compare (lifterWeight l1) (lifterWeight l2)
             | otherwise = EQ
