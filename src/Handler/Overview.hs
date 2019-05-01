{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Overview where

import Import

getOverviewR :: Handler Html
getOverviewR = defaultLayout $
  [whamlet|
    <p><a href=@{FrontendR True False}> Frontend  (mit Kari Update)
    <p><a href=@{FrontendR False False}> Frontend (ohne Kari Update)
    <p><a href=@{BeamerR}> Beamer
    <p><a href=@{AdminR}> Admin
    <p><a href=@{JuryR PLeft}> Seitenkampfrichter Links
    <p><a href=@{JuryR PMain}> Hauptkampfrichter
    <p><a href=@{JuryR PRight}> Seitenkampfrichter Rechts|]

