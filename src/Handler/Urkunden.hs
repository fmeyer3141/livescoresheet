{-# language NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Urkunden (getUrkundenR, postUrkundenR) where

import Import
import Handler.Admin
import ManageScoresheetState
import qualified Data.Text as T
import qualified Prelude as P
import Weightclass
import Ageclass
import PackedHandler
import Scoresheetlogic
import qualified Data.Foldable as F

classForm :: Class -> MForm Handler (FormResult (Maybe Class), Widget)
classForm cl = do
  (checkedRes, checkedView) <- mreq checkBoxField (FieldSettings "" Nothing Nothing Nothing []) Nothing
  let clRes = (\b -> if b then Just cl else Nothing) <$> checkedRes
  pure (clRes, [whamlet| <p>
                           #{show cl}: ^{fvInput checkedView} |])

classesForm :: [Class] -> Html -> MForm Handler (FormResult [Class], Widget)
classesForm cls extra = do
  forms <- mapM classForm cls
  let res = map catMaybes $ sequenceA $ fst <$> forms
  let widget = [whamlet| #{extra}|] *> F.sequenceA_ (snd <$> forms)
               *> [whamlet| <button type=submit> Submit|]
  pure (res,widget)

getUrkundenR :: Handler Html
getUrkundenR = do
  classesFromDB <- getClasses <$> atomicallyUnpackHandler getLiftersFromDB
  (classesFormWidget, classesFormEnctype) <- generateFormPost $ classesForm $ sort classesFromDB
  defaultLayout $
    [whamlet| <form #classForm enctype=#{classesFormEnctype} method=post action=@{UrkundenR}>
                ^{classesFormWidget} |]

getPlacingStr :: Lifter -> Int -> Text
getPlacingStr l@Lifter{..} pl =
  case (lifterOutOfCompetition, getTotalLifter l) of
    (True, _)        -> "a.K." -- a.K.
    (False, Just _)  -> (pack $ show pl) `T.append` ". Platz"
    (False, Nothing) -> "DQ" --DQ

showLifterRawUrkunde :: Lifter -> Text
showLifterRawUrkunde l = if lifterRaw l then "Raw" else "Equipped"

showLifterWeightclassUrkunde :: Lifter -> Text
showLifterWeightclassUrkunde l = case lifterWeightclass l of
  Plusclass  w -> "über " `T.append` (T.pack $ show w) `T.append` "Kg"
  Minusclass w -> "bis "  `T.append` (T.pack $ show w) `T.append` "Kg"

postUrkundenR :: Handler TypedContent
postUrkundenR = do
  lifters <- atomicallyUnpackHandler getLiftersFromDB
  let classesFromDB = getClasses lifters
  ((res,_),_) <- runFormPost $ classesForm classesFromDB
  case res of
    FormFailure fs  -> invalidArgs fs
    FormMissing     -> invalidArgs ["classesForm is missing"]
    FormSuccess cls -> do
      let liftersFiltered = liftersWithPlacings $ filter ((flip elem) cls . getClass) lifters
      let lifterCSV =  (++) identifiers $ T.concat $ P.map (T.concat .
                       map
                       (\(pl,l@Lifter {..}) -> T.intercalate "," [ lifterName, lifterClub, printPrettyAgeclass $ lifterAgeclass, showLifterWeightclassUrkunde l
                                                                 , getPlacingStr l pl, showTotal l, showWilks lifterSex (getTotalLifter l) lifterWeight
                                                                 , showLifterRawUrkunde l, pack $ show lifterSex] ++ "\n"))
                       liftersFiltered -- :: Text
      return $ TypedContent "text/csv" $ toContent lifterCSV

       where
         identifiers = "Name,Club,Ageclass,Weightclass,Placing,Total,Wilks,Raw,Sex\n"
