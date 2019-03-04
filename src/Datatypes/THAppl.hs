{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module THAppl where
import MeetTypes
import Database.Persist.TH

$(meetTypeTH)
$(resultsTypeTH)
$(test2)
derivePersistField ("Results'")
