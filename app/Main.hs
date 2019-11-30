module Main (main) where

import Data.List.Split (wordsBy)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering))
import System.IO.Echo (withoutInputEcho)

data Reverse = Reverse [[String]] ([String], String, [String]) [[String]]

data Action
  = Split Char
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  -- | Unsplit
  -- | Divide

integrate :: ([String], String, [String]) -> [String]
integrate (befores, current, afters) = reverse befores ++ [current] ++ afters

derive :: [String] -> ([String], String, [String])
derive [] = ([], "", [])
derive (x : xs) = ([], x, xs)

act :: Action -> Reverse -> Reverse
act (Split c) (Reverse beforeL (beforeW, current, afterW) afterL) =
  let (s, ss) = case wordsBy (== c) current of
                  [] -> (current, [])
                  x : xs -> (x, xs)
  in Reverse beforeL (beforeW, s, ss ++ afterW) afterL
act MoveUp r@(Reverse [] _ _) = r
act MoveUp (Reverse (before : befores) current afters) =
  Reverse befores (derive before) (integrate current : afters)
act MoveDown r@(Reverse _ _ []) = r
act MoveDown (Reverse befores current (after : afters)) =
  Reverse (integrate current : befores) (derive after) afters
act MoveLeft r@(Reverse _ ([], _, _) _) = r
act MoveLeft (Reverse beforeLs ((beforeW : beforeWs), current, afterWs) afterLs) =
  Reverse beforeLs (beforeWs, beforeW, current : afterWs) afterLs
act MoveRight r@(Reverse _ (_, _, []) _) = r
act MoveRight (Reverse beforeLs (beforeWs, current, afterW : afterWs) afterLs) =
  Reverse beforeLs (current : beforeWs, afterW, afterWs) afterLs

loop :: Reverse -> IO ()
loop r = do
  c <- getChar
  if c == 'q'
  then return ()
  else loop (act MoveDown r)

main :: IO ()
main = withoutInputEcho do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  case args of
    [input] -> do
      text <- readFile input
      let (current, rest) = case lines text of
                              [] -> ("", [])
                              x : xs -> (x, xs)
      let r = Reverse [] ([], current, []) $ fmap pure rest
      loop r
    _ -> die "Unexpected command-line arguments"
