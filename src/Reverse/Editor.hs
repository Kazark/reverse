module Reverse.Editor (Action(..), act, recognizeAction) where

import Reverse.Base
import Reverse.Model

data Action
  = Split Char
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Accent
  -- | Unsplit
  -- | Divide

recognizeAction :: Monad m => m Char -> m (Maybe Action)
recognizeAction getChr = do
  c <- getChr
  case c of
    'k'  -> return $ Just MoveUp
    'j'  -> return $ Just MoveDown
    'h'  -> return $ Just MoveLeft
    'l'  -> return $ Just MoveRight
    '\'' -> return $ Just Accent
    'q'  -> return Nothing
    's'  -> fmap (Just . Split) getChr
    _    -> recognizeAction getChr

act :: Action -> Reverse -> Reverse
act Accent (Reverse beforeL (beforeW, current, afterW) afterL) =
  let newCurrent = current { accented = not (accented current) }
  in Reverse beforeL (beforeW, newCurrent, afterW) afterL
act (Split c) (Reverse beforeL (beforeW, current, afterW) afterL) =
  let (s, ss) = case splitOn c current of
                  [] -> (current, [])
                  x : xs -> (x, xs)
  in Reverse beforeL (beforeW, s, Row ss <> afterW) afterL
act MoveUp r@(Reverse [] _ _) = r
act MoveUp (Reverse (before : befores) current afters) =
  Reverse befores (derive (cursorIndex current) before) (integrate current : afters)
act MoveDown r@(Reverse _ _ []) = r
act MoveDown (Reverse befores current (after : afters)) =
  Reverse (integrate current : befores) (derive (cursorIndex current) after) afters
act MoveLeft r@(Reverse _ (Row [], _, _) _) = r
act MoveLeft (Reverse beforeLs (Row (beforeW : beforeWs), current, afterWs) afterLs) =
  Reverse beforeLs (Row beforeWs, beforeW, Row [current] <> afterWs) afterLs
act MoveRight r@(Reverse _ (_, _, Row []) _) = r
act MoveRight (Reverse beforeLs (beforeWs, current, Row (afterW : afterWs)) afterLs) =
  Reverse beforeLs (Row [current] <> beforeWs, afterW, Row afterWs) afterLs
