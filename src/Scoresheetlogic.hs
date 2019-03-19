{-# Language NoImplicitPrelude #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoresheetlogic where

import Import
import MeetTypesTH
import THApplStage1
import THApplStage2
import Weightclass
import qualified Prelude as P
import Ageclass
import Sex
import Data.Maybe

type Class = (Ageclass, Sex, Weightclass, Bool)

data Plate = Plate25 | Plate20 | Plate15 | Plate10 | Plate5 | Plate2_5 | Plate1_25 deriving (Show, Enum)

instance ToJSON Plate where
  toJSON = toJSON . show

isDQ :: Lifter -> Bool
isDQ = or . fmap (and . fmap (\a -> if attemptFail a then True else False) . attemptsAsList) . resultList . lifterRes

getTotalLifter :: Lifter -> Maybe Double
getTotalLifter lifter@(Lifter {..}) =
  case isDQ lifter of
    True -> Nothing
    False -> Just . sum . fmap (fromMaybe 0.0 . getBestAttempt) $ resultList lifterRes

nextAttemptNr :: MeetState -> Lifter -> Maybe Int
nextAttemptNr s l
  | (attemptPending $ att1 d)  = Just 1
  | (attemptPending $ att2 d)  = Just 2
  | (attemptPending $ att3 d)  = Just 3
  | otherwise = Nothing

  where
    d :: Discipline
    d = getDisciplineFromLifter (meetStateCurrDiscipline s) l

nextWeight:: MeetState -> Lifter -> Maybe Int -> Maybe Double
nextWeight s l att
  | att == Just 1    = attemptWeight $ att1 d
  | att == Just 2    = attemptWeight $ att2 d
  | att == Just 3    = attemptWeight $ att3 d
  | otherwise        = Nothing

  where
    d = getDisciplineFromLifter (meetStateCurrDiscipline s) l

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


cmpLifterGroupAndOrder :: MeetState -> Lifter -> Lifter -> Ordering
cmpLifterGroupAndOrder s l1 l2 = case cmpLifterGroup (meetStateCurrGroupNr s) l1 l2 of
                                   EQ -> cmpLifterOrder s l1 l2
                                   x -> x

cmpLifterOrder :: MeetState -> Lifter -> Lifter -> Ordering
cmpLifterOrder s l1 l2
             | attemptNrl1 /= attemptNrl2
                   = compareMaybe attemptNrl1 attemptNrl2
             | weightl1 /= weightl2
                   = compareMaybe weightl1 weightl2
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
        attemptNrl1 = nextAttemptNr s l1
        attemptNrl2 = nextAttemptNr s l2
        weightl1 = nextWeight s l1 $ attemptNrl1
        weightl2 = nextWeight s l2 $ attemptNrl2

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
getNextLifterInGroup :: MeetState -> [Lifter] -> Maybe Lifter
getNextLifterInGroup s lifterlist = listToMaybe nextLifters
  where
    nextLifters = filter (\l -> Nothing /= nextWeight s l (nextAttemptNr s l))
                     $ sortBy (cmpLifterOrder s)
                     $ filter ((==) (meetStateCurrGroupNr s) . lifterGroup) lifterlist


