{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BlockArguments #-}

module THApplStage2 where
import MeetTypesTH
import THApplStage1
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import ClassyPrelude.Yesod
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

import qualified Control.Monad as M

$(meetTypeTH)
$(emptyResultsTH)

meetDiscs :: [String]
meetDiscs = map (firstLowercase . T.unpack . fst) meetType

firstLowercase :: String -> String
firstLowercase (x:xs) = (C.toLower x):xs
firstLowercase xs     = xs

firstUppercase :: String -> String
firstUppercase (x:xs) = (C.toUpper x):xs
firstUppercase xs     = xs

dbLifterTypes :: String
dbLifterTypes = getLifterString "Lifter'" "" ++ "\n" ++ getLifterString "LifterBackup'" "version Int"
  where
    getLifterString name additional =
      (name ++ "\n  " ++ additional ++  "\n  name Text\n  lot Int\n  age Int\n  sex Bool\n  ageclass Int\n  weightclass Int\n"
       ++ "  weight Double\n  raw Bool\n  group Int\n" :: String) ++ thGenerated ++ final
    final = "\n  club Text\n  deriving Eq\n  deriving Show\n"

    thGenerated = L.intercalate "\n" $ do
      d <- meetDiscs
      a <- [1..3]
      (p,t) <- [(1, "Int"), (2, "Double"), (3, "UTCTime")]
      pure ("  "++ (getDisciplineResultFieldName d a p) ++" "++t)

getDisciplineResultFieldName :: String -> Int -> Int -> String
getDisciplineResultFieldName d anr inr = d ++ "Att" ++ show anr ++ "D" ++ show inr

databaseScheme :: Q Exp
databaseScheme = do
  let q = quoteExp persistLowerCase
  file <- liftIO $ readFile "config/models"
  let text = T.unpack $ decodeUtf8 file
  let models = text ++ dbLifterTypes
  q models

dbLifterConvFunctions :: Q [Dec]
dbLifterConvFunctions = do
  firstN@[nName, nLot, nAge, nSex, nAgeclass, nWeightclass, nWeight, nRaw, nGroup]
    <- mapM newName ["lName", "lLot", "lAge", "lSex", "lAgeclass", "lWeightclass", "lWeight", "lRaw", "lGroup"]
  lClub  <- newName "lClub"
  lRes  <- newName "lRes"

-- used for pattern matching on discipline
  let attInfo      = mapM newName ["attState", "attWeight", "attTime"]
  let attempts     = M.replicateM 3 attInfo
  disciplines <- M.replicateM (length meetDiscs) attempts :: Q [[[Name]]]

  let allGenNames = concat $ concat disciplines :: [Name]

  let patternVars = VarP <$> (firstN ++ allGenNames ++ [lClub])
  let firstConv  = [ VarE nName, VarE nLot, VarE nAge, AppE (VarE $ mkName "sexFromDB") $ VarE nSex
                   , AppE (VarE $ mkName "ageclassFromDB") $ VarE nAgeclass
                   , AppE (VarE $ mkName "weightclassFromDB") $ VarE nWeightclass , VarE nWeight
                   , VarE nRaw, VarE nGroup ]
  let firstConv'  = [ VarE nName, VarE nLot, VarE nAge, AppE (VarE $ mkName "sexToDB") $ VarE nSex
                    , AppE (VarE $ mkName "ageclassToDB") $ VarE nAgeclass
                    , AppE (VarE $ mkName "weightclassToDB") $ VarE nWeightclass , VarE nWeight
                    , VarE nRaw, VarE nGroup ]

  let pattern = ConP (mkName "Lifter'") $ patternVars

  let firstAppl exp' fConv = L.foldl' (\l p -> AppE l p) exp' fConv
  let disciplinesAppl = AppE (firstAppl (ConE $ mkName "Lifter") firstConv) $
                         L.foldl (\d dp -> AppE d $  L.foldl' applAttempt (ConE $ mkName "Discipline") dp)
                                 (ConE $ mkName "Results")
                                 disciplines
  let body = NormalB $ AppE disciplinesAppl (VarE lClub)
  let clause = [Clause [pattern] body []]

  versionVar <- newName "version"
  let patternBackup = ConP (mkName "LifterBackup'") $ (VarP versionVar):patternVars
  let bodyBackup = NormalB $ AppE (AppE (ConE $ mkName "LifterBackup") (AppE disciplinesAppl (VarE lClub))) $
                    VarE versionVar
  let clauseBackup = [Clause [patternBackup] bodyBackup []]

  let toLifter'Pattern = ConP (mkName "Lifter") $ VarP <$> firstN ++ [lRes, lClub]
  let applDisciplines c d = M.foldM (applAttempt' d lRes) c [1..3]
  let applResults conE = M.foldM applDisciplines conE meetType
  applResults' <- applResults $ firstAppl (ConE $ mkName "Lifter'") firstConv'
  let toLifter'Body = NormalB $ AppE applResults' (VarE lClub)
  let toLifter'Clause = [Clause [toLifter'Pattern] toLifter'Body []]

  let toLifterBackup'Pattern = ConP (mkName "LifterBackup") $
                                [ ConP (mkName "Lifter") $ VarP <$> firstN ++ [lRes, lClub]
                                , VarP versionVar ]
  applResults'' <- applResults $ firstAppl (AppE (ConE $ mkName "LifterBackup'") (VarE versionVar)) firstConv'
  let toLifterBackup'Body = NormalB $ AppE applResults'' (VarE lClub)
  let toLifterBackup'Clause = [Clause [toLifterBackup'Pattern] toLifterBackup'Body []]

  let updateLifter'LIdV = mkName "lId"
  let updateLifter'GroupV = mkName "lGroup"
  let updateLifter'ResV = mkName "lResults"
  let updateLifter'Vars = [updateLifter'LIdV, updateLifter'GroupV, updateLifter'ResV]
  let updateLifter'Pattern = VarP <$> updateLifter'Vars
  let updateLifter'Group = applPersistAss "Lifter'Group" (VarE updateLifter'GroupV)
  updateLifter'UpdateOps <- createAllLetsAndUpdateOps (VarE updateLifter'ResV)
                              >>= (pure . createFinalUpdateExp [updateLifter'Group])
  let updateLifter'Body = NormalB $ AppE (AppE (VarE $ mkName "updateWhere")
                            (ListE [InfixE (Just $ ConE $ mkName "Lifter'Id")
                                      (VarE $ mkName "==.") (Just $ VarE updateLifter'LIdV)]))
                            updateLifter'UpdateOps
  let updateLifter'Clause = [Clause updateLifter'Pattern updateLifter'Body []]

  pure
    [ FunD (mkName "updateLifter'") updateLifter'Clause
    , FunD (mkName "toLifter") clause, FunD (mkName "toLifterBackup") clauseBackup
    , FunD (mkName "toLifter'") toLifter'Clause
    , FunD (mkName "toLifterBackup'") toLifterBackup'Clause]

    where
      applAttempt :: Exp -> [Name] -> Exp
      applAttempt e ai = AppE e (L.foldl' (\a i -> AppE a (VarE i)) (VarE $ mkName "attemptFromDB") ai)

      applAttempt' :: MeetTypeEntry -> Name -> Exp -> Int -> Q Exp
      applAttempt' d rName c aNr = do
        let discName = T.unpack $ "_disc" ++ fst d
        localNames <- mapM newName ["i1", "i2", "i3"]
        let tupP = VarP <$> localNames
        let letBody = NormalB $
                        AppE (VarE $ mkName "attemptToDB")
                          (AppE (VarE $ mkName $ "att" ++ show aNr)
                            (AppE (VarE $ mkName discName) (VarE rName)))

        let letAppl = L.foldl' (\c' n -> AppE c' (VarE n)) c localNames
        pure $ LetE [ValD (TupP tupP) letBody []] letAppl

      applPersistAss :: String -> Exp -> Exp
      applPersistAss n e2 = InfixE (Just $ ConE $ mkName n) (VarE $ mkName "=.") (Just e2)

      createFinalUpdateExp :: [Exp] -> [(Exp -> Exp, [Exp])] -> Exp
      createFinalUpdateExp additional l =
        let (f, es) = L.foldl1' (\(f',es') (f'',es'') -> (f' . f'', es' ++ es'')) l :: (Exp -> Exp, [Exp]) in
        f $ ListE (es ++ additional)

      createAllLetsAndUpdateOps :: Exp -> Q [(Exp -> Exp, [Exp])]
      createAllLetsAndUpdateOps resV =
        sequenceA $ do -- [Q [Exp]]
          -- list monad
          discName <- firstUppercase <$> meetDiscs
          aNr      <- [1..3]

          pure $ do
            -- Q Monad
            localNames <- mapM newName ["i1", "i2", "i3"]
            let tupP = VarP <$> localNames
            let letBody = NormalB $
                            AppE (VarE $ mkName "attemptToDB")
                              (AppE (VarE $ mkName $ "att" ++ show aNr)
                                (AppE (VarE $ mkName $ "_disc" ++ discName) resV))

            pure $ (,) (LetE [ValD (TupP tupP) letBody []]) $ do -- Q Exp
              -- list monad
              i        <- [1..3]
              let fieldName = "lifter'" ++ firstUppercase (getDisciplineResultFieldName discName aNr i)
              let dbUpdateOpName = firstUppercase fieldName
              pure $ applPersistAss dbUpdateOpName $ VarE (localNames P.!! (i-1))
