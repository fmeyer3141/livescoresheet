{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

module THApplStage1 where
import qualified Data.Text as T
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

-- (DisciplineName, Lens' )
newtype Lens'NT s a= Lens'NT { unpackLens'NT :: Lens' s a }
type MeetTypeEntry = (Text, Lens'NT Results Discipline)
type MeetType      = [MeetTypeEntry]

meetTypeTH :: Q [Dec]
meetTypeTH = do
    discs <- liftIO readDisciplines
    let meetTypeName = mkName "meetType"
    let conName      = mkName "MeetType"
    pure $
      [ SigD meetTypeName (ConT conName)
      , ValD (VarP meetTypeName) (NormalB (ListE (tuples $ T.unpack <$> discs)) ) [] ]

    where
      genTuple discName = TupE [ LitE (StringL discName), AppE (ConE $ mkName "Lens'NT") $ VarE (mkName ("disc" ++ discName)) ]
      tuples discs = map genTuple discs

apFlipped :: Applicative f => f a -> f (a -> b) -> f b
apFlipped = flip (<*>)

emptyResultsTH :: Q [Dec]
emptyResultsTH = do
    discs <- liftIO readDisciplines
    let emptyResultsName    = mkName "emptyResults"
    let resultsName         = mkName "Results"
    let apps = genApps discs
    pure $
      [ SigD emptyResultsName (AppT (AppT ArrowT $ ConT $ mkName "AttemptTime") $ ConT resultsName)
      , ValD (VarP emptyResultsName) (NormalB apps) []]

    where
      genApps []     = AppE (VarE $ mkName "pure")  $ ConE (mkName "Results")
      genApps (_:ds) = AppE (AppE (VarE $ mkName "apFlipped") (VarE $ mkName "emptyDiscipline")) $ genApps ds
