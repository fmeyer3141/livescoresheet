{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module THApplStage2 where
import THApplStage1

$(meetTypeTH)
