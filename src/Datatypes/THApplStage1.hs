{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module THApplStage1 where
import qualified Data.Text as T
import qualified Prelude as P
import qualified Data.Foldable as F
import ClassyPrelude.Yesod
import Language.Haskell.TH
import MeetTypesTH
import Settings

$(resultsTypeTH)
$([d| deriving instance Show Results |])
$([d| deriving instance Read Results |])
$([d| deriving instance Eq Results |])
derivePersistField ("Results")

data MeetType = MeetType { unpackMeet :: [(Text, Results -> Discipline)] }
instance Show MeetType where
  show (MeetType m) = "MeetType " ++ (show $ fst <$> m)

meetTypeTH :: Q [Dec]
meetTypeTH = do
    discs <- liftIO readDisciplines
    let meetTypeName = mkName "meetType"
    pure . pure $
      ValD (VarP meetTypeName) (NormalB (AppE (ConE $ mkName "MeetType") (ListE (tuples $ T.unpack <$> discs)))) []

    where
      genTuple discName = TupE [LitE (StringL discName), VarE (mkName ("disc" ++ discName)) ]
      tuples discs = map genTuple discs

emptyResultsTH :: Q [Dec]
emptyResultsTH = do
    discs <- liftIO readDisciplines
    let emptyResultsName    = mkName "emptyResults"
    let emptyDisciplineName = mkName "emptyDiscipline"
    let resultsName         = mkName "Results"
    let apps = F.foldl' (\a n -> AppE a n) (ConE resultsName) $ P.replicate (length discs) (VarE emptyDisciplineName)
    pure . pure $
      ValD (VarP emptyResultsName) (NormalB apps) []

    where
      genTuple discName = TupE [LitE (StringL discName), VarE (mkName ("disc" ++ discName)) ]
      tuples discs = map genTuple discs
