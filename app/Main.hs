module Main (main) where

import Reverse.Model
import Reverse.Editor
import Reverse.UI (TermEnv, initTerm, redraw, resetScreen)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Echo (withoutInputEcho)

loop :: TermEnv -> Reverse -> IO ()
loop env r = do
  redraw env r
  c <- recognizeAction getChar
  case c of
    Nothing -> resetScreen env
    Just action -> loop env $ act action r

main :: IO ()
main = withoutInputEcho do
  -- Do the terminal initialization as the very first thing, so we know
  -- immediately if there are problems.
  env <- initTerm
  args <- getArgs
  case args of
    [input] -> do
      text <- readFile input
      loop env $ ingest text
    _ -> die "Unexpected command-line arguments"
