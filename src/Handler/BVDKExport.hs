{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.BVDKExport where

import Import hiding ((!!))
import Prelude ((!!))
import Common
import ManageScoresheetState
import Scoresheetlogic
import Yesod.Form.Bootstrap3
import PackedHandler
import qualified Data.Map as M
import qualified Data.Text as T (pack, intercalate)

import Control.Lens ((&), (.~), ix)

csvForm :: Form FileInfo
csvForm = renderBootstrap3 BootstrapBasicForm $
            fileAFormReq "Wählen Sie die Anmeldungsdatei aus."

getBVDKExportR :: Handler Html
getBVDKExportR = do
  (csvFormWidget, csvFormEnctype) <- generateFormPost csvForm
  defaultLayout
    [whamlet|
      <form method=post action=@{BVDKExportR} enctype=#{csvFormEnctype}>
        ^{csvFormWidget}
        <button type=submit>
          Upload It!|]

transformRow :: Map Text (Int,Lifter) -> [Text] -> [Text]
transformRow lMap row =
  let lName = (row!!4 ++ " " ++ row!!3) in
  case lookup lName lMap of
    Nothing -> ["Not found " ++ lName]
    Just (pl, l@Lifter{..}) ->
      let sqDisc    = getDisciplineFromLifter "Squat"    l in
      let benchDisc = getDisciplineFromLifter "Bench"    l in
      let deadDisc  = getDisciplineFromLifter "Deadlift" l in

      row
      & ix 13 .~ show' lifterWeight -- BodyWeight
      & ix 14 .~ showWilks lifterSex (getTotalLifter l) lifterWeight -- Wilks
      & ix 15 .~ show' lifterLot -- Lot Number
      & ix 16 .~ showAttempt (att1 sqDisc) -- Squat 1
      & ix 17 .~ showAttempt (att2 sqDisc) -- Squat 2
      & ix 18 .~ showAttempt (att3 sqDisc) -- Squat 3
      & ix 19 .~ showAttempt (att1 benchDisc) -- Bench 1
      & ix 20 .~ showAttempt (att2 benchDisc) -- Bench 2
      & ix 21 .~ showAttempt (att3 benchDisc) -- Bench 3
      & ix 22 .~ showAttempt (att1 deadDisc) -- Dead 1
      & ix 23 .~ showAttempt (att2 deadDisc) -- Dead 2
      & ix 24 .~ showAttempt (att3 deadDisc) -- Dead 3
      & ix 25 .~ maybe "D.Q." show' (getTotalLifter l) -- Total
      & ix 26 .~ showWilks lifterSex (getTotalLifter l) lifterWeight -- Punkte (Wilks again?)
      & ix 27 .~ show' pl -- Platz

  where
    show' :: Show a => a -> Text
    show' = T.pack . show

postBVDKExportR :: Handler TypedContent
postBVDKExportR = do
  ((result, _), _) <- runFormPost csvForm
  case result of
    FormSuccess info -> do
      csvFull <- liftIO $ parseCSV (fileSource info)
      let (header:csv) = csvFull
      lifters <- atomicallyUnpackHandler $ concat . liftersWithPlacings <$> getLiftersFromDB
      let lifterMap = M.fromList $ ((lifterName . snd) &&& id) <$> lifters
      let res = map (transformRow lifterMap) csv
      pure $ TypedContent "text/csv" $ toContent $
        T.intercalate "," header ++ "\n" ++ T.intercalate "\n" (T.intercalate "," <$> res)

    FormFailure fs   -> invalidArgs fs
    _                -> invalidArgs ["Form is mising"]
