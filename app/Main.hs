module Main (main) where

import Reverse.Editor
import Reverse.Editor.Mode (ModeUI(..))
import Reverse.UI (ViewModel, TermEnv, initTerm, resetScreen)
import qualified Reverse.UI as UI
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Echo (withoutInputEcho)
import Reverse.Help (printHelp)

modeUI :: TermEnv -> ModeUI IO ViewModel
modeUI env =
  ModeUI { redraw = UI.redraw env
         , userInputChar = getChar
         }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printHelp
    [input] -> withoutInputEcho do
      env <- initTerm
      text <- readFile input
      runEditor (modeUI env) text
      resetScreen env
    _ -> die "Unexpected command-line arguments"
