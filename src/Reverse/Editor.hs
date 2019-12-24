{-# LANGUAGE RankNTypes #-}
module Reverse.Editor (runEditor) where

import Control.Monad (forM_)
import Reverse.Editor.Mode
import Reverse.Editor.NormalMode
import Reverse.Editor.CombineMode
import Reverse.UI (ViewModel)

runEditor :: Monad z => (forall s. ViewModel s => ModeUI z s) -> String -> z ()
runEditor ui = runNormal . ingest where
  runNormal state = do
    o <- runMode normalMode ui state
    forM_ o runCombine
  runCombine state = do
    o <- runMode combineMode ui state
    runNormal o
