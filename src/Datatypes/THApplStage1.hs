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
type MeetTypeEntry = ∀ f. Functor f => (Text, (Discipline -> (f Discipline)) -> Results -> (f Results))
type MeetType      = ∀ f. Functor f => [(Text, (Discipline -> (f Discipline)) -> Results -> (f Results))]

fstMeetType :: (Text, (Discipline -> (Identity Discipline)) -> Results -> (Identity Results)) -> Text
fstMeetType = fst

meetTypeTH :: Q [Dec]
meetTypeTH = do
    discs <- liftIO readDisciplines
    let meetTypeName = mkName "meetType"
    let conName      = mkName "MeetType"
    pure $
      [ SigD meetTypeName (ConT conName)
      , ValD (VarP meetTypeName) (NormalB (ListE (tuples $ T.unpack <$> discs)) ) [] ]

    where
      genTuple discName = TupE [ LitE (StringL discName),VarE (mkName ("disc" ++ discName)) ]
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
      [ SigD emptyResultsName (AppT (AppT ArrowT $ ConT $ mkName "UTCTime") $ ConT resultsName)
      , ValD (VarP emptyResultsName) (NormalB apps) []]

    where
      genApps []     = AppE (VarE $ mkName "pure")  $ ConE (mkName "Results")
      genApps (_:ds) = AppE (AppE (VarE $ mkName "apFlipped") (VarE $ mkName "emptyDiscipline")) $ genApps ds
