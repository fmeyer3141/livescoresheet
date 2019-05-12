{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Jury where

import Import
import Yesod.WebSockets
import qualified Data.Text as T
import qualified Data.List as L
import Control.Lens ((^.), (%~))
import Control.Monad.Logger
import Data.Singletons
import Scoresheetlogic
import ManageScoresheetState
import PackedHandler
import Misc

import SocketHelper (getLifterAttemptInfo)

colorForm :: Html -> MForm Handler (FormResult (RefereeDecision p), Widget)
colorForm = renderDivs $
  RefereeDecision <$> areq checkBoxField redFormat Nothing
                  <*> areq checkBoxField blueFormat Nothing
                  <*> areq checkBoxField yellowFormat Nothing
  where
    redFormat    = FieldSettings "R"  Nothing (Just "cbRedCard")    Nothing [("class", "cbJuryCard")]
    blueFormat   = FieldSettings "B" Nothing (Just "cbBlueCard")   Nothing [("class", "cbJuryCard")]
    yellowFormat = FieldSettings "Y" Nothing (Just "cbYellowCard") Nothing [("class", "cbJuryCard")]

prettyPrintPos :: RefereePlaces -> Text
prettyPrintPos PLeft  = "Seitenkampfrichter links"
prettyPrintPos PMain  = "Hauptkampfrichter"
prettyPrintPos PRight = "Seitenkampfrichter rechts"

sendKariData :: FrontendMessage -> Maybe Value
sendKariData (JuryFrontendInfoMessage v) = Just v
sendKariData _                           = Nothing

-- Did the Post work?
getJuryR' :: RefereePlaces -> Maybe Bool -> Handler Html
getJuryR' p mb = do
  webSockets $ dataSocket sendKariData
  (colorFormWidget, colorFormEnctype) <- generateFormPost colorForm
  defaultLayout $ do
    setTitle $ toHtml (prettyPrintPos p)
    case mb of
      Just True -> [whamlet| Die Daten wurden gespeichert |]
      Just False -> [whamlet| Konnte Versuchsergebnis nicht eintragen |]
      Nothing -> pure ()

    $(widgetFile "kari")

getJuryR :: RefereePlaces -> Handler Html
getJuryR p  = getJuryR' p Nothing

allDecEntered :: RefereeResult -> Bool
allDecEntered (RefereeResult (Just _, Just _, Just _)) = True
allDecEntered _                                        = False

getCurrELifter :: MeetState -> [(Key Lifter', Lifter)] -> Maybe (Key Lifter', Lifter)
getCurrELifter ms els = (getNextLiftersWithf snd ms els) !! 0


unpackRefereeDecision :: RefereeDecision p -> (Bool,Bool,Bool)
unpackRefereeDecision d = (red d,blue d,yellow d)

markLift :: UTCTime -> RefereeResult -> PackedHandler (Maybe (PackedHandler LifterAttemptInfo))
markLift t (RefereeResult (Just le, Just ma, Just ri)) = do
  let weight = sum $ map (\(r,b,y) -> if null $ filter id [r,b,y] then 1 else -1)
                         ([unpackRefereeDecision le, unpackRefereeDecision ma, unpackRefereeDecision ri]) :: Int

  meetState <- getCurrMeetStateFromDB
  elifters <- getELiftersInGroupFromDB (meetStateCurrGroupNr meetState)

  let eCurrLifter = getCurrELifter meetState elifters
  let currDiscipline = meetStateCurrDiscipline meetState

  pure $ case eCurrLifter of
    Just (eId, l) ->
      do
        attemptNr        <- nextAttemptNr meetState l
        discLens         <- map snd $ L.find ((==) currDiscipline . fst) meetType
        let attempt       = getAttempt attemptNr $ (lifterRes l) ^. (unpackLens'NT discLens)
        attW             <- attemptWeight attempt
        let lifterAttInfo = getLifterAttemptInfo meetState l attW

        if weight > 0 then
          -- Valid
          markLiftDBHelper (eId, l) attempt attemptNr discLens True lifterAttInfo
        else
          -- Invalid
          markLiftDBHelper (eId, l) attempt attemptNr discLens False lifterAttInfo

    _       -> Nothing

    where
      markLiftDBHelper :: (Key Lifter', Lifter) -> Attempt -> AttemptNr -> Lens'NT Results Discipline -> Bool ->
                          LifterAttemptInfo -> Maybe (PackedHandler LifterAttemptInfo)
      markLiftDBHelper el a an lsNT b attInfo =
        let ls = unpackLens'NT lsNT in
        let (eId, l) = el in
        do
          mA         <- markAttempt t b a
          let updResults = ls %~ (setDiscipline an mA) $ lifterRes l
          pure $ updateLiftersInDB
            [(eId, l {lifterRes = updResults })] *> pure attInfo

markLift _ _ = pure Nothing

-- TODO: is there a better way too do this?
postJuryR :: RefereePlaces -> Handler Html
postJuryR p  = withSomeSing p (postJuryR')

postJuryR' :: SRefereePlaces a -> Handler Html
postJuryR' p = do
  ((res,_), _) <- runFormPost colorForm
  case res of
    FormSuccess colors -> (atomicallyUnpackHandler $ do
      time <- liftIO getCurrentTime
      ioRef <- appRefereeState <$> getYesodPacked
      refereeState <- atomicModifyIORef' ioRef $
        \s -> let checkForReset r = if allDecEntered r then (emptyRefereeResult,r) else (r,r) in
        checkForReset $ updateRefereeResultByPos p (Just colors) s
        --case p of
          --PLeft  -> updateRefereeResultByPos --s { refereeLeft  = Just colors }
          --PMain  -> s { refereeMain  = Just colors }
          --PRight -> s { refereeRight = Just colors }

      logInfoN $ "Referee State: " ++ (T.pack $ show refereeState)
      if allDecEntered refereeState then
        logInfoN "All referee decisions entered"
        *> (markLift time refereeState >>=
             (\markRes -> case markRes of
               Just act ->
                (act >>= (\lAttInfo -> pushRefereeStateToChannel (Just lAttInfo, refereeState, True)))
                *> pure True--perform marking action
               Nothing  -> logInfoN "Error marking Lifter" *> pure False)) -- TODO log error
      else
        pushRefereeStateToChannel (Nothing, refereeState, False) *> pure True)

     >>= (getJuryR' (fromSing p) . Just) -- show success

    FormFailure (t:_) -> defaultLayout [whamlet| Error #{t}|]
    _                 -> defaultLayout [whamlet| Unknown Form Error |]

getResetKariR :: Handler Html
getResetKariR = do
  ioRef <- appRefereeState <$> getYesod
  atomicallyUnpackHandler . packHandler $ atomicModifyIORef' ioRef $ const ( emptyRefereeResult
                                                                           , ())
  redirect AdminR
