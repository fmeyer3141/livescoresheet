{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module THApplStage1 where
import qualified Data.Text as T
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
    discs <- liftIO $ (meetTypeStr <$> meetSettings)
    let meetTypeName = mkName "meetType"
    -- [d| meetType = MeetType [("Squat", discSquat)]|]
    pure . pure $
      ValD (VarP meetTypeName) (NormalB (AppE (ConE $ mkName "MeetType") (ListE (tuples $ T.unpack <$> discs)))) []

    where
      genTuple discName = TupE [LitE (StringL discName), VarE (mkName ("disc" ++ discName)) ]
      tuples discs = map genTuple discs
