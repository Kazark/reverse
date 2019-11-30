module Reverse.Editor (Action(..), act, recognizeAction) where

import Reverse.Model
import Data.List.Split (wordsBy)

data Action
  = Split Char
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  -- | Unsplit
  -- | Divide

recognizeAction :: Monad m => m Char -> m (Maybe Action)
recognizeAction getChr = do
  c <- getChr
  case c of
    'k' -> return $ Just MoveUp
    'j' -> return $ Just MoveDown
    'h' -> return $ Just MoveLeft
    'l' -> return $ Just MoveRight
    'q' -> return Nothing
    's' -> fmap (Just . Split) getChr
    _   -> recognizeAction getChr

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
