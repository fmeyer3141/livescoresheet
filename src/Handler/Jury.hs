{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Jury where

import Import
import Yesod.WebSockets
import qualified Data.Text as T
import qualified Data.List as L
import Control.Lens ((^.), (%%~))

import Scoresheetlogic
import ManageScoresheetState
import PackedHandler
import Misc

colorForm :: Html -> MForm Handler (FormResult RefereeDecision, Widget)
colorForm = renderDivs $
  RefereeDecision <$> areq checkBoxField "Rot: " Nothing <*> areq checkBoxField "Blau: " Nothing
                  <*> areq checkBoxField "Gelb:" Nothing

prettyPrintPos :: RefereePlaces -> Text
prettyPrintPos PLeft  = "Seitenkampfrichter Links"
prettyPrintPos PMain  = "Hauptkampfrichter"
prettyPrintPos PRight = "Seitenkampfrichter Rechts"

computeKariData :: FrontendMessage -> Maybe Value
computeKariData (LifterUpdate (ms, lifters)) = Just . toJSON $
                                               (getNextLifterInGroup ms lifters >>= getLifterAttemptInfo ms)
computeKariData _                            = Nothing

-- Did the Post work?
getJuryR' :: RefereePlaces -> Maybe Bool -> Handler Html
getJuryR' p mb = do
  webSockets $ dataSocket computeKariData
  (colorFormWidget, colorFormEnctype) <- generateFormPost colorForm
  defaultLayout $ do
    setTitle $ toHtml (prettyPrintPos p)
    case mb of
      Just True -> [whamlet| Die Daten wurden gespeichert |]
      Just False -> [whamlet| Konnte Versuchsergebnis nicht eintragen |]
      Nothing -> pure ()

    $(widgetFile "kari")

getJuryR :: RefereePlaces -> Handler Html
getJuryR p = getJuryR' p Nothing

allDecEntered :: RefereeResult -> Bool
allDecEntered (RefereeResult (Just _) (Just _) (Just _)) = True
allDecEntered _                                          = False

setDiscipline :: Int -> Attempt -> Discipline -> Maybe Discipline
setDiscipline 1 att d = Just $ d {att1 = att}
setDiscipline 2 att d = Just $ d {att2 = att}
setDiscipline 3 att d = Just $ d {att3 = att}
setDiscipline _ _   _ = Nothing

getCurrELifter :: MeetState -> [(Key Lifter', Lifter)] -> Maybe (Key Lifter', Lifter)
getCurrELifter ms els = (getNextLiftersWithf snd ms els) !! 0

markAttempt :: UTCTime -> Bool -> Attempt -> Maybe Attempt
markAttempt t True  = validateAttempt t
markAttempt t False = inValidateAttempt t

getAttempt :: Int -> Discipline -> Maybe Attempt
getAttempt 1 d = Just $ att1 d
getAttempt 2 d = Just $ att2 d
getAttempt 3 d = Just $ att3 d
getAttempt _ _ = Nothing

markLift :: UTCTime -> RefereeResult -> PackedHandler (Maybe (PackedHandler LifterAttemptInfo))
markLift t (RefereeResult (Just le) (Just ma) (Just ri)) = do
  let weight = sum $ map (\(RefereeDecision r b y) -> if null $ filter id [r,b,y] then 1 else -1)
                         [le,ma,ri] :: Int
  elifters  <- getELiftersFromDB
  meetState <- getCurrMeetStateFromDB
  let eCurrLifter = getCurrELifter meetState elifters
  let currDiscipline = meetStateCurrDiscipline meetState

  pure $ case eCurrLifter of
    Just (eId, l) ->
      do
        attemptNr     <- nextAttemptNr meetState l
        discLens      <- map snd $ L.find ((==) currDiscipline . fst) meetType
        attempt       <- getAttempt attemptNr $ (lifterRes l) ^. (unpackLens'NT discLens)
        lifterAttInfo <- getLifterAttemptInfo meetState l

        if weight > 0 then
          -- Valid
          markLiftDBHelper (eId, l) attempt attemptNr discLens True lifterAttInfo
        else
          -- Invalid
          markLiftDBHelper (eId, l) attempt attemptNr discLens False lifterAttInfo

    _       -> Nothing
    -- liftIO $ putStrLn $ T.pack $ "An error occurred. Lifter could not be found in DB and marked. NextLifters: " ++ show (getNextLifters meetState (fEntityVal elifters)) ++
    --                               " nextELifters: " -- ++ show (getNextLiftersWithf fromEntity meetState elifters)

    where
      markLiftDBHelper :: (Key Lifter', Lifter) -> Attempt -> Int -> Lens'NT Results Discipline -> Bool ->
                          LifterAttemptInfo -> Maybe (PackedHandler LifterAttemptInfo)
      markLiftDBHelper el a an lsNT b attInfo =
        let ls = unpackLens'NT lsNT in
        let (eId, l) = el in
        do
          mA         <- markAttempt t b a
          updResults <- ls %%~ (setDiscipline an mA) $ lifterRes l
          pure $ updateLiftersInDB
            [(eId, l {lifterRes = updResults })] *> pure attInfo

markLift _ _ = pure Nothing

postJuryR :: RefereePlaces -> Handler Html
postJuryR p = do
  ((res,_), _) <- runFormPost colorForm
  case res of
    FormSuccess colors -> (atomicallyUnpackHandler $ do
      time <- liftIO getCurrentTime
      ioRef <- appRefereeState <$> getYesodPacked
      refereeState <- atomicModifyIORef' ioRef $
        \s -> let checkForReset r = if allDecEntered r then (emptyRefereeResult,r) else (r,r) in
        checkForReset $ case p of
          PLeft  -> s { refereeLeft  = Just colors }
          PMain  -> s { refereeMain  = Just colors }
          PRight -> s { refereeRight = Just colors }

      liftIO $ putStrLn $ "Referee State: " ++ (T.pack $ show refereeState)
      if allDecEntered refereeState then
        liftIO (putStrLn "All decisions entered")
        *> (markLift time refereeState >>=
             (\markRes -> case markRes of
               Just act ->
                (act >>= (\lAttInfo -> pushRefereeStateToChannel (Just lAttInfo, refereeState, True)))
                *> pure True--perform marking action
               Nothing  -> liftIO (putStrLn "Error marking Lifter") *> pure False)) -- TODO log error
      else
        pushRefereeStateToChannel (Nothing, refereeState, False) *> pure True)

     >>= (getJuryR' p . Just) -- show success

    FormFailure (t:_) -> defaultLayout [whamlet| Error #{t}|]
    _                 -> defaultLayout [whamlet| Unknown Form Error |]

getResetKariR :: Handler Html
getResetKariR = do
  ioRef <- appRefereeState <$> getYesod
  atomicallyUnpackHandler . packHandler $ atomicModifyIORef' ioRef $ const ( emptyRefereeResult
                                                                           , ())
  redirect AdminR
