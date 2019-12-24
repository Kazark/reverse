{-# LANGUAGE MultiParamTypeClasses #-}
module Reverse.Editor.NormalMode
  ( Normal(..), Action(..), ingest
  ) where

import Data.List.Split (wordsBy)
import Reverse.Contexted
import Reverse.Editor.Mode
import Reverse.Model
import Reverse.UI (ViewModel(..))

newtype Normal = Normal (Model Cell)

data Action
  = Split Char
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Accent

splitOn :: Char -> Cell -> [Cell]
splitOn chr curCell =
  let mkCell cc = curCell { cellContents = cc }
  in fmap mkCell $ wordsBy (== chr) $ cellContents curCell

ingest :: String -> Normal
ingest text =
  let (current', a) =
        case splitOn ' ' . normal <$> lines text of
          [] -> ([normal ""], [])
          x : xs -> (x, xs)
      c = InContext { befores = []
                    , current = focusOn 0 current'
                    , afters = fmap Row a
                    }
  in Normal c

moveDown :: (Focused b, Contexted b a) => InContext a b -> InContext a b
moveDown c =
  case afters c of
    [] -> c
    newFocus : newAfters ->
      InContext { befores = blur (current c) : befores c
                , current = focusOn (focusedOn (current c)) newFocus
                , afters = newAfters
                }

moveUp :: InContext (Row Cell) (InContext Cell Cell)
        -> InContext (Row Cell) (InContext Cell Cell)
moveUp = oneEighty . moveDown . oneEighty

nAct :: Action -> Model Cell -> Model Cell
nAct Accent ctxtd = fmap (fmap (\c -> c { accented = not (accented c) })) ctxtd
nAct (Split c) ctxtd =
   fmap (\ctxt ->
     let (s, ss) = case splitOn c (current ctxt) of
                     [] -> (current ctxt, [])
                     x : xs -> (x, xs)
     in ctxt { current = s, afters = ss <> afters ctxt }
   ) ctxtd
nAct MoveUp ctxtd = moveUp ctxtd
nAct MoveDown ctxtd = moveDown ctxtd
nAct MoveLeft ctxtd = fmap before ctxtd
nAct MoveRight ctxtd = fmap after ctxtd

instance Mode Normal Action where
  recognizeAction getChr = do
    c <- getChr
    case c of
      'k'  -> return $ Just MoveUp
      'j'  -> return $ Just MoveDown
      'h'  -> return $ Just MoveLeft
      'l'  -> return $ Just MoveRight
      '\'' -> return $ Just Accent
      -- TODO should we really allow split on any character?
      -- We don't intend to allow join on any character. We should probably
      -- allow join and split on the same subset.
      's'  -> fmap (Just . Split) getChr
      'q'  -> return Nothing
      _    -> recognizeAction getChr

  act a (Normal n) = Normal $ nAct a n

instance ViewModel Normal where
  content (Normal c) = blur c
  cursorColumn (Normal c) = focusedOn $ current c
  cursorRow (Normal c) = length $ befores c
