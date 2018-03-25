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

cmpLifterGroupAndTotal :: Lifter -> Lifter -> Ordering
cmpLifterGroupAndTotal l1 l2 |(lifterGroup l1 /= lifterGroup l2) = compare (lifterGroup l1) (lifterGroup l2)
                             |((lifterGroup l1 == lifterGroup l2) && (getTotalLifter l1 /= getTotalLifter l2))
                               = (flip compare) (getTotalLifter l1) (getTotalLifter l2)
                             |((lifterGroup l1 == lifterGroup l2) && (getTotalLifter l1 == getTotalLifter l2))
                               = compare (lifterWeight l1) (lifterWeight l2)
cmpLifterGroupAndTotal _ _ = EQ
