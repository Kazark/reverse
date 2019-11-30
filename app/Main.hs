module Main (main) where

import Reverse.Model
import Reverse.Editor
import Reverse.UI (TermEnv, initTerm, redraw)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering))
import System.IO.Echo (withoutInputEcho)

loop :: TermEnv -> Reverse -> IO ()
loop env r = do
  c <- getChar
  if c == 'q'
  then return ()
  else do
    redraw env r
    loop env $ act MoveDown r

main :: IO ()
main = withoutInputEcho do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  case args of
    [input] -> do
      text <- readFile input
      let (current, rest) =
            case lines text of
              [] -> ("", [])
              x : xs -> (x, xs)
      let r = Reverse [] ([], current, []) $ fmap pure rest
      env <- initTerm
      loop env r
    _ -> die "Unexpected command-line arguments"
