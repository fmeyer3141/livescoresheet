{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Jury where

import Import
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe
import Control.Lens (over, view)

import Scoresheetlogic
import Handler.Admin

colorForm :: Html -> MForm Handler (FormResult RefereeDecision, Widget)
colorForm = renderDivs $
  RefereeDecision <$> areq checkBoxField "Rot: " Nothing <*> areq checkBoxField "Blau: " Nothing
                  <*> areq checkBoxField "Gelb:" Nothing

prettyPrintPos :: RefereePlaces -> Text
prettyPrintPos PLeft  = "Seitenkampfrichter Links"
prettyPrintPos PMain  = "Hauptkampfrichter"
prettyPrintPos PRight = "Seitenkampfrichter Rechts"

-- Did the Post work?
getJuryR' :: RefereePlaces -> Maybe Bool -> Handler Html
getJuryR' p mb = do
  (colorFormWidget, colorFormEnctype) <- generateFormPost $ colorForm
  defaultLayout $ do
    case mb of
      Just True -> [whamlet| Die Daten wurden gespeichert |]
      Just False -> [whamlet| eerrrrrorrrr |]
      Nothing -> pure ()
    [whamlet|
      <h1> Kamprichterposition: #{prettyPrintPos p}
      <form method=post action@{AdminR} enctype=#{colorFormEnctype}>
        ^{colorFormWidget}
        <button type=submit> Absenden
    |]

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

getCurrELifter :: MeetState -> [Entity Lifter] -> Maybe (Entity Lifter)
getCurrELifter ms els = (getNextLiftersWithf fromEntity ms els) !! 0

markAttempt :: Bool -> Attempt -> Maybe Attempt
markAttempt True  = validateAttempt
markAttempt False = inValidateAttempt

getAttempt :: Int -> Discipline -> Maybe Attempt
getAttempt 1 d = Just $ att1 d
getAttempt 2 d = Just $ att2 d
getAttempt 3 d = Just $ att3 d
getAttempt _ _ = Nothing

markLift :: RefereeResult -> Handler ()
markLift (RefereeResult (Just le) (Just ma) (Just ri)) = do
  let weight = sum $ map (\(RefereeDecision r b y) -> if null $ filter id [r,b,y] then 1 else -1)
                         [le,ma,ri] :: Int
  elifters <- getELiftersFromDB
  meetState <- getCurrMeetStateFromDB
  let eCurrLifter = getCurrELifter meetState elifters
  let currDiscipline = meetStateCurrDiscipline meetState
  let discLensM = snd . fromJust $ L.find ((==) currDiscipline . fst) meetType
  let discLensV = snd . fromJust $ L.find ((==) currDiscipline . fst) meetType

  case eCurrLifter of
    Just (Entity eId l) ->
      let attemptNr = fromJust $ nextAttemptNr meetState l in
      let attempt = fromJust $ getAttempt attemptNr (view discLensV (lifterRes l)) in
      if weight > 0 then do
        -- Valid
        markLiftDBHelper (Entity eId l) attempt attemptNr discLensM True
      else
        -- Invalid
        markLiftDBHelper (Entity eId l) attempt attemptNr discLensM False

    _       -> liftIO $ putStrLn $ T.pack $ "An error occurred. Lifter could not be found in DB and marked. NextLifters: " ++ show (getNextLifters meetState (fromEntities elifters)) ++
                                   " nextELifters: " -- ++ show (getNextLiftersWithf fromEntity meetState elifters)

    where
      markLiftDBHelper el a an ls b =
        let (Entity eId l) = el in
        let mA = fromJust $ markAttempt b a in
        updateLiftersInDB
          [Entity eId (l {lifterRes = over ls (fromJust . setDiscipline an mA) (lifterRes l)} )]

markLift _                                = liftIO $ putStrLn "An error ocurred lift could not be marked"

postJuryR :: RefereePlaces -> Handler Html
postJuryR p = do
  ((res,_), _) <- runFormPost colorForm
  case res of
    FormSuccess colors -> do
      ioRef <- appRefereeState <$> getYesod
      refereeState <- atomicModifyIORef ioRef $
        \s -> let checkForReset r = if allDecEntered r then (emptyRefereeResult,r) else (r,r) in
        checkForReset $ case p of
          PLeft  -> s { refereeLeft  = Just colors }
          PMain  -> s { refereeMain  = Just colors }
          PRight -> s { refereeRight = Just colors }

      liftIO $ putStrLn $ "Referee State: " ++ (T.pack $ show refereeState)
      if allDecEntered refereeState then do
        liftIO $ putStrLn "All decisions entered"
        markLift refereeState
        pushDataFromDBToChannel
        pushRefereeStateToChannel (refereeState, True) -- let the frontend show the state
      else
        pure ()

      getJuryR' p $ Just True

    FormFailure (t:_) -> defaultLayout [whamlet| Error #{t}|]
    _                 -> defaultLayout [whamlet| Unknown Form Error |]
