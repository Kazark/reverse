module Reverse.Editor
  ( Action(..), act, recognizeAction
  ) where

import Data.Bifunctor (second)
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

--data Mode
--  = Normal Normal

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

act :: Action -> Normal -> Normal
act Accent r = second (second (\c -> c { accented = not (accented c) })) r
act (Split c) r =
  second (\(Contexted beforeW current afterW) ->
    let (s, ss) = case splitOn c current of
                    [] -> (current, [])
                    x : xs -> (x, xs)
    in Contexted beforeW s (Row ss <> afterW)
  ) r
act MoveUp r@(Contexted [] _ _) = r
act MoveUp (Contexted (b : bs) current as) =
  Contexted bs (derive (cursorIndex current) b) (integrate current : as)
act MoveDown r@(Contexted _ _ []) = r
act MoveDown (Contexted bs current (a : as)) =
  Contexted (integrate current : bs) (derive (cursorIndex current) a) as
act MoveLeft r@(Contexted _ (Contexted (Row []) _ _) _) = r
act MoveLeft (Contexted beforeLs (Contexted (Row (beforeW : beforeWs)) current afterWs) afterLs) =
  Contexted beforeLs (Contexted (Row beforeWs) beforeW $ Row [current] <> afterWs) afterLs
act MoveRight r@(Contexted _ (Contexted _ _ (Row [])) _) = r
act MoveRight (Contexted beforeLs (Contexted beforeWs current (Row (afterW : afterWs))) afterLs) =
  Contexted beforeLs (Contexted (Row [current] <> beforeWs) afterW $ Row afterWs) afterLs
