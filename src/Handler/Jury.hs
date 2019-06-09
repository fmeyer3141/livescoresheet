{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Handler.Jury where

import Import
import Yesod.WebSockets
import qualified Data.Text as T
import qualified Data.List as L
import Control.Lens ((^.), (%~))
import Control.Monad.Logger
import Data.Singletons
import Scoresheetlogic
import Data.Time.Clock.POSIX
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
    redFormat    = FieldSettings ""  Nothing (Just "cbRedCard")   Nothing [("class", "cbJuryCard")]
    blueFormat   = FieldSettings "" Nothing (Just "cbBlueCard")   Nothing [("class", "cbJuryCard")]
    yellowFormat = FieldSettings "" Nothing (Just "cbYellowCard") Nothing [("class", "cbJuryCard")]

prettyPrintPos :: RefereePlaces -> Text
prettyPrintPos PLeft  = "Seitenkampfrichter links"
prettyPrintPos PMain  = "Hauptkampfrichter"
prettyPrintPos PRight = "Seitenkampfrichter rechts"

sendKariData :: FrontendMessage -> Maybe Value
sendKariData (JuryFrontendInfoMessage v) = Just v
sendKariData _                           = Nothing

-- Did the Post work?
-- Nothing -> Nothing marked last time
-- Just Nothing -> Error
-- Just $ Just attInfo -> success
getJuryR' :: RefereePlaces -> Maybe (Maybe LifterAttemptInfo) -> Handler Html
getJuryR' p mb = do
  webSockets $ dataSocket sendKariData
  (colorFormWidget, colorFormEnctype) <- generateFormPost colorForm
  defaultLayout $ do
    setTitle $ toHtml (prettyPrintPos p)
    case mb of
      Just (Just attInfo) -> [whamlet| Die Fehlerkarten wurden gespeichert für #{lifterAttemptInfoName attInfo}|]
      Just (Nothing) -> [whamlet| Konnte Versuchsergebnis nicht eintragen |]
      Nothing -> pure ()
    $(widgetFile "kari")

getJuryR :: RefereePlaces -> Handler Html
getJuryR p  = getJuryR' p Nothing

getCurrELifter :: MeetState -> [(Key Lifter', Lifter)] -> Maybe (Key Lifter', Lifter)
getCurrELifter ms els = (getNextLiftersWithf snd ms els) !! 0

unpackRefereeDecision :: RefereeDecision p -> (Bool,Bool,Bool)
unpackRefereeDecision d = (red d,blue d,yellow d)

getLifterInfoFromDB :: PackedHandler (Maybe LifterAttemptInfo)
getLifterInfoFromDB = do
  meetState <- getCurrMeetStateFromDB
  elifters <- getELiftersInGroupFromDB (meetStateCurrGroupNr meetState)
  pure $ do
    l <- snd <$> getCurrELifter meetState elifters
    attemptNr        <- nextAttemptNr meetState l
    let attempt       = getAttempt attemptNr $ getDisciplineFromLifter (meetStateCurrDiscipline meetState) l
    attW             <- attemptWeight attempt
    pure $ getLifterAttemptInfo meetState l attW

type FinalRefereeDecision = (RefereeDecision 'PLeft, RefereeDecision 'PMain, RefereeDecision 'PRight)

markLift :: AttemptTime -> FinalRefereeDecision -> PackedHandler (Maybe LifterAttemptInfo)
markLift t (le, ma, ri) = do
  let weight = sum $ map (\(r,b,y) -> if null $ filter id [r,b,y] then 1 else -1)
                         ([unpackRefereeDecision le, unpackRefereeDecision ma, unpackRefereeDecision ri]) :: Int

  meetState <- getCurrMeetStateFromDB
  elifters <- getELiftersInGroupFromDB (meetStateCurrGroupNr meetState)
  let eCurrLifter = getCurrELifter meetState elifters
  let currDiscipline = meetStateCurrDiscipline meetState

  maybe (pure Nothing) (\(act, aInfo) -> act *> pure (pure aInfo)) $ do
    (eId,l)          <- eCurrLifter
    attemptNr        <- nextAttemptNr meetState l
    Lens'NT discLens <- map snd $ L.find ((==) currDiscipline . fst) meetType
    let attempt       = getAttempt attemptNr $ (lifterRes l) ^. discLens
    attW             <- attemptWeight attempt
    markedAtt        <- markAttempt t (weight > 0) attempt

    let updResults = discLens %~ (setDiscipline attemptNr markedAtt) $ lifterRes l
    pure (updateLiftersInDB [(eId, l {lifterRes = updResults })], getLifterAttemptInfo meetState l attW)

postJuryR :: RefereePlaces -> Handler Html
postJuryR pl = withSomeSing pl $ \p -> do
  ((res,_), _) <- runFormPost colorForm
  case res of
    FormSuccess colors ->
      do
        markRes <- atomicallyUnpackHandler $ do
          time <- realToFrac <$> liftIO getPOSIXTime
          ioRef <- appRefereeState <$> getYesodPacked
          refereeState <- atomicModifyIORef' ioRef $ resetHelper . updateRefereeResultByPos p (Just colors)
          logInfoN $ "Referee State: " ++ (T.pack $ show refereeState)
          case refereeState of
            (s,Just (l,m,r)) -> do
              logInfoN "All referee decisions entered"
              markRes <- markLift time (l,m,r)
              case markRes of
                Just lAttInfo -> pushRefereeStateToChannel (Just lAttInfo, s, True)
                Nothing  -> logInfoN "Error marking Lifter"
              pure markRes
            (s,_) -> pushRefereeStateToChannel (Nothing, s, False) *> getLifterInfoFromDB

        getJuryR' (fromSing p) $ Just markRes -- show success

    FormFailure (t:_) -> defaultLayout [whamlet| Error #{t}|]
    _                 -> defaultLayout [whamlet| Unknown Form Error |]

  where
    resetHelper :: RefereeResult -> (RefereeResult, (RefereeResult, Maybe FinalRefereeDecision))
    resetHelper rr@(RefereeResult (Just l, Just m, Just r)) = (emptyRefereeResult, (rr, Just (l,m,r)))
    resetHelper s                                           = (s,(s,Nothing))

getResetKariR :: Handler Html
getResetKariR = do
  ioRef <- appRefereeState <$> getYesod
  atomicallyUnpackHandler . packHandler $ atomicModifyIORef' ioRef $ const (emptyRefereeResult, ())
  redirect AdminR

isMainReferee :: RefereePlaces -> Bool
isMainReferee PMain = True 
isMainReferee _ = False