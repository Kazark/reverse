module Reverse.Editor (runEditor) where

import Control.Monad (forM_)
import Data.Functor.Contravariant (contramap)
import Reverse.Editor.CombineMode
import Reverse.Editor.Contexted
import Reverse.Editor.Mode
import Reverse.Editor.Model
import Reverse.Editor.NormalMode
import Reverse.Editor.Selection (selectionOffset)
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
  ViewModel { content = blur $ cancel x
            , cursorColumn = focusedOn $ current x
            , cursorRow = length $ befores x
            , selection = Just ( focusedOn $ current x
                               , selectionOffset $ current $ current x
                               )
            }

recognize :: Monad z => z Char -> [(Char, a)] -> z a
recognize getChr aMap = do
  c <- getChr
  case lookup c aMap of
    Nothing -> recognize getChr aMap
    Just x -> return x

runMode :: Monad z => Mode s o a -> ModeUI z s -> s -> z o
runMode mode ui state = do
  redraw ui state
  action <- recognize (userInputChar ui) (actionMap mode)
  either return (runMode mode ui) (react mode action state)

runEditor :: Monad z => ModeUI z ViewModel -> String -> z ()
runEditor ui = runNormal . ingest where
  runNormal state = do
    o <- runMode normalMode (contramap normalViewModel ui) state
    forM_ o runCombine
  runCombine state = do
    o <- runMode combineMode (contramap combineViewModel ui) state
    runNormal o
