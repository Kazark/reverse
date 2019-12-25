module Reverse.Editor (runEditor) where

import Control.Monad (forM_)
import Data.Functor.Contravariant (contramap)
import Reverse.Editor.CombineMode
import Reverse.Editor.Contexted
import Reverse.Editor.Mode
import Reverse.Editor.Model
import Reverse.Editor.NormalMode
import Reverse.UI (ViewModel(..))

normalViewModel :: NormalModel -> ViewModel
normalViewModel x =
  ViewModel { content = blur x
            , cursorColumn = focusedOn $ current x
            , cursorRow = length $ befores x
            , selection = Nothing
            }

combineViewModel :: CombineModel -> ViewModel
combineViewModel x =
  -- Much bad
  -- So disfunction
  -- Very crap
  -- Wow
  ViewModel { content = blur $ cancel x
            , cursorColumn = focusedOn $ current x
            , cursorRow = length $ befores x
            , selection = Just (focusedOn $ current x, focusedOn (current x) + 2)
            }

runEditor :: Monad z => ModeUI z ViewModel -> String -> z ()
runEditor ui = runNormal . ingest where
  runNormal state = do
    o <- runMode normalMode (contramap normalViewModel ui) state
    forM_ o runCombine
  runCombine state = do
    o <- runMode combineMode (contramap combineViewModel ui) state
    runNormal o
