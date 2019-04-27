{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Overview where

import Import

getOverviewR :: Handler Html
getOverviewR = defaultLayout $
  [whamlet|
    <p><a href=@{AdminR}> Admin
    <p><a href=@{JuryR PLeft}> Seitenkampfrichter Links
    <p><a href=@{JuryR PMain}> Hauptkampfrichter
    <p><a href=@{JuryR PRight}> Seitenkampfrichter Rechts|]

