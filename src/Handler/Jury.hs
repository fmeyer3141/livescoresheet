{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Jury where

import Import

getJuryR :: RefereePlaces -> Handler Html
getJuryR p = defaultLayout [whamlet| Nothing to see here |]

postJuryR :: RefereePlaces -> Handler Html
postJuryR p = defaultLayout [whamlet| Nothing to see here |]
