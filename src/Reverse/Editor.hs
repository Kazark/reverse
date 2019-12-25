module Reverse.Editor (runEditor) where

import Control.Monad (forM_)
import Data.Functor.Contravariant (contramap)
import qualified Data.List.NonEmpty as NEL
import Reverse.Editor.CombineMode
import Reverse.Editor.Contexted
import Reverse.Editor.Mode
import Reverse.Editor.Model
import Reverse.Editor.NormalMode
import Reverse.UI (ViewModel(..), CursorInRow(..))

normalViewModel :: NormalModel -> ViewModel
normalViewModel x =
  ViewModel { content = blur x
            , cursorInRow = NormalCursor $ focusedOn $ current x
            , cursorRow = length $ befores x
            }

combineViewModel :: CombineModel -> ViewModel
combineViewModel x =
  ViewModel { content =
                blur $ fmap (\c ->
                  c { current = NEL.head $ current c
                    , afters = NEL.tail (current c) <> afters c
                    }
                ) x
            , cursorInRow = NormalCursor $ focusedOn $ current x
            , cursorRow = length $ befores x
            }

runEditor :: Monad z => ModeUI z ViewModel -> String -> z ()
runEditor ui = runNormal . ingest where
  runNormal state = do
    o <- runMode normalMode (contramap normalViewModel ui) state
    forM_ o runCombine
  runCombine state = do
    o <- runMode combineMode (contramap combineViewModel ui) state
    runNormal o
