{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module THApplStage1 where
import qualified Data.Text as T
import qualified Prelude as P
import qualified Data.Foldable as F
import ClassyPrelude.Yesod
import Language.Haskell.TH
import MeetTypesTH
import Control.Lens

$(resultsTypeTH)
$([d| deriving instance Show Results |])
$([d| deriving instance Read Results |])
$([d| deriving instance Eq Results |])
derivePersistField ("Results")
makeLenses ''Results

type ViewFunc = Results -> Discipline
type OverFunc = (Discipline -> Discipline) -> Results -> Results
data MeetType = MeetType { unpackMeet :: [(Text, ViewFunc, OverFunc)] }

meetTypeTH :: Q [Dec]
meetTypeTH = do
    discs <- liftIO readDisciplines
    let meetTypeName = mkName "meetType"
    let conName      = mkName "MeetType"
    pure $
      [ SigD meetTypeName (ConT conName)
      , ValD (VarP meetTypeName) (NormalB (AppE (ConE $ mkName "MeetType") (ListE (tuples $ T.unpack <$> discs))) ) [] ]

    where
      genTuple discName = TupE [ LitE (StringL discName), AppE (VarE $ mkName "view") $ VarE (mkName ("disc" ++ discName))
                               , AppE (VarE $ mkName "over") $ VarE (mkName ("disc" ++ discName))]
      tuples discs = map genTuple discs

emptyResultsTH :: Q [Dec]
emptyResultsTH = do
    discs <- liftIO readDisciplines
    let emptyResultsName    = mkName "emptyResults"
    let emptyDisciplineName = mkName "emptyDiscipline"
    let resultsName         = mkName "Results"
    let apps = F.foldl' (\a n -> AppE a n) (ConE resultsName) $ P.replicate (length discs) (VarE emptyDisciplineName)
    pure $
      [ SigD emptyResultsName (ConT resultsName)
      , ValD (VarP emptyResultsName) (NormalB apps) []]
