module Main (main) where

import Reverse.Base
import Reverse.Model
import Reverse.Editor
import Reverse.UI (TermEnv, initTerm, redraw)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Echo (withoutInputEcho)

loop :: TermEnv -> Reverse -> IO ()
loop env r = do
  redraw env r
  c <- recognizeAction getChar
  case c of
    Nothing -> return ()
    Just action -> loop env $ act action r

main :: IO ()
main = withoutInputEcho do
  args <- getArgs
  case args of
    [input] -> do
      text <- readFile input
      let (current, rest) =
            case lines text of
              [] -> ("", [])
              x : xs -> (x, xs)
      let r = Reverse [] (Row [], current, Row []) $ fmap (Row . pure) rest
      env <- initTerm
      loop env r
    _ -> die "Unexpected command-line arguments"
