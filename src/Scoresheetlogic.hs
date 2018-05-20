{-# Language NoImplicitPrelude #-}

module Scoresheetlogic where

import Import
import Weightclass
import qualified Prelude as P
import qualified Data.List as L
import Ageclass
import Sex

type Class = (Ageclass, Sex, Weightclass, Bool)

data Plate = Plate25 | Plate20 | Plate15 | Plate10 | Plate5 | Plate2_5 | Plate1_25 deriving (Show, Enum)

instance ToJSON Plate where
  toJSON = toJSON . show

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

nextAttemptNr :: Lifter -> Maybe Int
nextAttemptNr l
            | lifterAttemptDL1Success l == Nothing   = Just 1
            | lifterAttemptDL2Success l == Nothing   = Just 2
            | lifterAttemptDL3Success l == Nothing   = Just 3
            | otherwise = Nothing

nextWeight:: Lifter -> Maybe Int -> Maybe Double
nextWeight l att
            | att == Just 1    = lifterAttemptDL1Weight l
            | att == Just 2    = lifterAttemptDL2Weight l
            | att == Just 3    = lifterAttemptDL3Weight l
            | otherwise        = Nothing

cmpLifterGroup :: Int -> Lifter -> Lifter -> Ordering
cmpLifterGroup g l1 l2 | (lifterGroup l1 == g) && (lifterGroup l2 /= g) -- nur l1 in prio gruppe
                             = LT
                       | (lifterGroup l2 == g) && (lifterGroup l1 /= g) -- nur l2 in prio gruppe
                             = GT
                       | lifterGroup l1 /= lifterGroup l2
                             = compare (lifterGroup l1) (lifterGroup l2) -- keiner in der prio Groupe
                       | otherwise -- Lifter in der selben nicht prio gruppe
                             = EQ

cmpLifterGroupAndTotal :: Int -> Lifter -> Lifter -> Ordering
cmpLifterGroupAndTotal g l1 l2 = case cmpLifterGroup g l1 l2 of
                                   EQ -> if getTotalLifter l1 /= getTotalLifter l2
                                         then -- sortiere nach total
                                           compare (lifterWeight l1) (lifterWeight l2)
                                         else -- dann nach BW
                                           flip compare (getTotalLifter l1) (getTotalLifter l2)

                                   x  -> x

cmpLifterTotalAndBw :: Lifter -> Lifter -> Ordering
cmpLifterTotalAndBw l1 l2   | getTotalLifter l1 /= getTotalLifter l2
                                  = flip compare (getTotalLifter l1) (getTotalLifter l2)
                            | otherwise --selbe Gruppe und Total identisch -> BW
                                  = compare (lifterWeight l1) (lifterWeight l2)


cmpLifterGroupAndOrder :: Int -> Lifter -> Lifter -> Ordering
cmpLifterGroupAndOrder g l1 l2 = case cmpLifterGroup g l1 l2 of
                                   EQ -> cmpLifterOrder l1 l2
                                   x -> x

cmpLifterOrder :: Lifter -> Lifter -> Ordering
cmpLifterOrder l1 l2
             | nextAttemptNr l1 /= nextAttemptNr l2
                   = compareMaybe (nextAttemptNr l1) (nextAttemptNr l2)
             | nextWeight l1 (nextAttemptNr l1) /= nextWeight l2 (nextAttemptNr l2)
                   = compareMaybe (nextWeight l1 $ nextAttemptNr l1)
                                                            (nextWeight l2 $ nextAttemptNr l2)
             | lifterWeight l1 /= lifterWeight l2
                   = compare (lifterWeight l1) (lifterWeight l2)
             | otherwise
                   = EQ
    where
        compareMaybe :: (Ord a) => Maybe a -> Maybe a -> Ordering -- Nothing also kein Gewicht angegeben oder alle Versuche gemacht -> ans ende sortieren
        compareMaybe Nothing Nothing   = EQ
        compareMaybe (Just _) Nothing  = LT
        compareMaybe Nothing (Just _)  = GT
        compareMaybe (Just x) (Just y) = compare x y

cmpLifterClass :: Lifter -> Lifter -> Ordering
cmpLifterClass l1 l2 | getClass l1 /= getClass l2
                           = compare (getClass l1) (getClass l2)
                     | getTotalLifter l1 /= getTotalLifter l2
                           = compare (getTotalLifter l1) (getTotalLifter l2)
                     | otherwise
                           = compare (lifterWeight l1) (lifterWeight l2)


cmpLifterClassPrio :: Class -> Lifter -> Lifter -> Ordering
cmpLifterClassPrio c l1 l2 | (c == getClass l1) && (c /= getClass l2)
                                 = LT
                           | (getClass l2 == c) && (getClass l1 /= c)
                                 = GT
                           | otherwise
                                 = cmpLifterClass l1 l2

getClass :: Lifter -> Class
getClass l = (lifterAgeclass l, lifterSex l, lifterWeightclass l, lifterRaw l)

getPlates:: Double -> [(Plate, Int)]
getPlates w = getPlateHelper Plate25 (w-25) -- Klemmen und Stange abziehen
  where
    numplates :: Double -> Double -> (Int, Double)
    numplates w1 p =
      let x = floor $ w1/p in (x, w1 - p* fromIntegral x)
    getPlateHelper :: Plate -> Double -> [(Plate, Int)]
    getPlateHelper Plate25   w1
      = let (n, w2) = numplates w1 (2*25) in (Plate25, n) : getPlateHelper Plate20 w2
    getPlateHelper Plate20   w1
      = let (n, w2) = numplates w1 (2*20) in (Plate20, n) : getPlateHelper Plate15 w2
    getPlateHelper Plate15   w1
      = let (n, w2) = numplates w1 (2*15) in (Plate15, n) : getPlateHelper Plate10 w2
    getPlateHelper Plate10   w1
      = let (n, w2) = numplates w1 (2*10) in (Plate10, n) : getPlateHelper Plate5 w2
    getPlateHelper Plate5    w1
      = let (n, w2) = numplates w1 (2*5) in (Plate5, n) : getPlateHelper Plate2_5 w2
    getPlateHelper Plate2_5  w1
      = let (n, w2) = numplates w1 (2*2.5) in (Plate2_5, n) : getPlateHelper Plate1_25 w2
    getPlateHelper Plate1_25 w1
      = let (n, _) = numplates w1 (2*1.25) in pure (Plate1_25, n)

-- LifterListe -> Gruppennr -> Nächster Lifter
getNextLifterInGroup :: [Lifter] -> Int -> Maybe Lifter
getNextLifterInGroup lifterlist groupNr = listToMaybe nextLifters
  where
    nextLifters = filter (\l -> Nothing /= nextWeight l (nextAttemptNr l))
                     $ sortBy cmpLifterOrder
                     $ filter ((==) groupNr . lifterGroup) lifterlist


