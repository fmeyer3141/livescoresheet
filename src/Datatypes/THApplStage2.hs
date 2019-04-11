{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module THApplStage2 where
import MeetTypesTH
import THApplStage1

$(meetTypeTH)
$(emptyResultsTH)
