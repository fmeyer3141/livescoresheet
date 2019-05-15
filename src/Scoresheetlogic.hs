{-# Language NoImplicitPrelude #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoresheetlogic where

import Import
import Weightclass
import Ageclass
import Sex

type Class = (Ageclass, Sex, Weightclass, Bool)

isDQ :: Lifter -> Bool
isDQ = or . map (and . map attemptFail . attemptsAsList) . resultList . lifterRes

type Comparison a = a -> a -> Ordering

infixr 8 .~.
(.~.) :: Comparison a -> Comparison a -> Comparison a
(.~.) cmp cmp' a a' = case cmp' a a' of
  EQ -> cmp a a'
  c  -> c

-- Running Total
getTotalLifter :: Lifter -> Maybe Double
getTotalLifter lifter@(Lifter {..}) =
  case isDQ lifter of
    True -> Nothing
    False -> Just . sum . map (fromMaybe 0.0 . getBestAttempt) $ resultList lifterRes

nextAttemptNr :: MeetState -> Lifter -> Maybe AttemptNr
nextAttemptNr s l
  | (attemptPending $ att1 d)  = Just Attempt1
  | (attemptPending $ att2 d)  = Just Attempt2
  | (attemptPending $ att3 d)  = Just Attempt3
  | otherwise = Nothing

  where
    d :: Discipline
    d = getDisciplineFromLifter (meetStateCurrDiscipline s) l

attWeight:: MeetState -> Lifter -> AttemptNr -> Maybe Double
attWeight s l = attemptWeight . (flip getAttempt) d
  where
    d = getDisciplineFromLifter (meetStateCurrDiscipline s) l

nextWeight :: MeetState -> Lifter -> Maybe Double
nextWeight ms l = nextAttemptNr ms l >>= attWeight ms l

cmpLifterGroup :: GroupNr -> Lifter -> Lifter -> Ordering
cmpLifterGroup g l1 l2 | (lifterGroup l1 == g) && (lifterGroup l2 /= g) -- nur l1 in prio gruppe
                             = LT
                       | (lifterGroup l2 == g) && (lifterGroup l1 /= g) -- nur l2 in prio gruppe
                             = GT
                       | lifterGroup l1 /= lifterGroup l2
                             = compare (lifterGroup l1) (lifterGroup l2) -- keiner in der prio Groupe
                       | otherwise -- Lifter in der selben nicht prio gruppe
                             = EQ

-- Größtes Total ist minimal hier -- dieser Lifter steht 'oben'
compareTotal :: Lifter -> Lifter -> Ordering
compareTotal l l' = (flip compare) (getTotalLifter l) (getTotalLifter l')

-- minimales Bodyweight ist minimal -- dieser Lifter steht 'oben'
compareBodyweight :: Lifter -> Lifter -> Ordering
compareBodyweight l l' = compare (lifterWeight l) (lifterWeight l')

cmpLifterTotalAndBw :: Lifter -> Lifter -> Ordering
cmpLifterTotalAndBw = compareBodyweight .~. compareTotal

cmpLifterGroupAndOrder :: MeetState -> Lifter -> Lifter -> Ordering
cmpLifterGroupAndOrder s = cmpLifterOrder s .~. cmpLifterGroup (meetStateCurrGroupNr s)

cmpLifterOrder :: MeetState -> Lifter -> Lifter -> Ordering
cmpLifterOrder s = (compare `on` lifterLot) .~. compareAttWeight .~. compareAttemptNr
    where
        compareMaybe :: (Ord a) => Maybe a -> Maybe a -> Ordering
        -- Nothing also kein Gewicht angegeben oder alle Versuche gemacht -> ans ende sortieren
        compareMaybe Nothing Nothing   = EQ
        compareMaybe (Just _) Nothing  = LT
        compareMaybe Nothing (Just _)  = GT
        compareMaybe (Just x) (Just y) = compare x y
        compareAttemptNr l l'          = compareMaybe (nextAttemptNr s l) (nextAttemptNr s l')
        compareAttWeight l l'          = compareMaybe (nextWeight s l) (nextWeight s l')

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

getNextLiftersWithf :: (a -> Lifter) -> MeetState -> [a] -> [a]
getNextLiftersWithf f s lifterList = filter (\l -> Nothing /= nextWeight s (f l))
                                     $ sortBy (\l l' -> cmpLifterOrder s (f l) (f l'))
                                     $ filter ((==) (meetStateCurrGroupNr s) . lifterGroup . f) lifterList

getNextLifters :: MeetState -> [Lifter] -> [Lifter]
getNextLifters = getNextLiftersWithf id

-- LifterListe -> Gruppennr -> Nächster Lifter
getNextLifterInGroup :: MeetState -> [Lifter] -> Maybe Lifter
getNextLifterInGroup ms l = (getNextLifters ms l) !! 0

getNext2LiftersInGroupWithf :: (a -> Lifter) -> MeetState -> [a] -> (Maybe a, Maybe a)
getNext2LiftersInGroupWithf f ms l = let n = getNextLiftersWithf f ms l in (n !! 0, n !! 1)

getNext2LiftersInGroup :: MeetState -> [Lifter] -> (Maybe Lifter, Maybe Lifter)
getNext2LiftersInGroup = getNext2LiftersInGroupWithf id

getPlacingWithfAndEq :: Eq b => (a -> Lifter) -> (a -> b) -> a -> [a] -> Placing
getPlacingWithfAndEq f eq el els =
  let liftersInClass      = filter ((==) (getClass $ f el) . getClass . f) els in
  let liftersWithPlacings = zip [1..] $ sortBy (cmpLifterTotalAndBw `on` f) liftersInClass in

  let mpl = headMay $ map fst $ filter ((==) (eq el) . eq . snd) liftersWithPlacings in
  fromMaybe 0 mpl

getPlacing :: Lifter -> [Lifter] -> Placing
getPlacing = getPlacingWithfAndEq id id

--compare on lifterkey to get a prognosed placing
getPlacingELifter :: ELifter -> [ELifter] -> Placing
getPlacingELifter = getPlacingWithfAndEq snd fst

showTotal :: Lifter -> Text
showTotal l = case getTotalLifter l of
                Just t -> pack $ show t
                Nothing -> "D.Q."
