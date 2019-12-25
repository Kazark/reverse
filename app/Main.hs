module Main (main) where

import Reverse.Editor
import Reverse.Editor.Mode (ModeUI(..))
import Reverse.UI (ViewModel, TermEnv, initTerm, resetScreen)
import qualified Reverse.UI as UI
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Echo (withoutInputEcho)

modeUI :: TermEnv -> ModeUI IO ViewModel
modeUI env =
  ModeUI { redraw = UI.redraw env
         , userInputChar = getChar
         }

main :: IO ()
main = withoutInputEcho do
  -- Do the terminal initialization as the very first thing, so we know
  -- immediately if there are problems.
  env <- initTerm
  args <- getArgs
  case args of
    [input] -> do
      text <- readFile input
      runEditor (modeUI env) text
      resetScreen env
    _ -> die "Unexpected command-line arguments"
