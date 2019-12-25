module Reverse.Editor.NormalMode ( NormalInput, normalMode, ingest ) where

import Data.List.Split (wordsBy)
import Reverse.Editor.Contexted
import Reverse.Editor.Delimiter
import Reverse.Editor.Mode
import Reverse.Editor.Model
import Reverse.Editor.Selection (select1)

data Action
  = Split Delim
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Accent

data NormalInput
  = Act Action
  | Combine
  | Quit

splitOn :: Char -> Cell -> [Cell]
splitOn chr curCell =
  let mkCell cc = curCell { cellContents = cc }
  in fmap mkCell $ wordsBy (== chr) $ cellContents curCell

ingest :: String -> NormalModel
ingest text =
  let (current', a) =
        case splitOn ' ' . normal <$> lines text of
          [] -> ([normal ""], [])
          x : xs -> (x, xs)
  in InContext { befores = []
               , current = focusOn 0 current'
               , afters = fmap Row a
               }

act :: Action -> NormalModel -> NormalModel
act Accent ctxtd = fmap (fmap (\c -> c { accented = not (accented c) })) ctxtd
act (Split c) ctxtd =
  fmap (\ctxt ->
    let (s, ss) = case splitOn (delimToChar c) (current ctxt) of
                    [] -> (current ctxt, [])
                    x : xs -> (x, xs)
    in ctxt { current = s, afters = ss <> afters ctxt }
  ) ctxtd
act MoveUp ctxtd = before ctxtd
act MoveDown ctxtd = after ctxtd
act MoveLeft ctxtd = fmap before ctxtd
act MoveRight ctxtd = fmap after ctxtd

initCombine :: NormalModel -> CombineModel
initCombine = fmap (fmap select1)

react' :: NormalInput -> NormalModel
       -> Either (Maybe CombineModel) NormalModel
react' Quit _ = Left Nothing
react' Combine model = Left $ Just $ initCombine model
react' (Act action) model = Right $ act action model

recognizeSplit :: Monad z => z Char -> z NormalInput
recognizeSplit getChr = do
  d <- getChr
  case charToDelim d of
    Nothing -> recognize' getChr
    Just d' -> return $ Act $ Split d'

recognize' :: Monad z => z Char -> z NormalInput
recognize' getChr = do
  c <- getChr
  case c of
    'q'  -> return Quit
    'c'  -> return Combine
    'k'  -> return $ Act MoveUp
    'j'  -> return $ Act MoveDown
    'h'  -> return $ Act MoveLeft
    'l'  -> return $ Act MoveRight
    '\'' -> return $ Act Accent
    's'  -> recognizeSplit getChr
    _    -> recognize' getChr

normalMode :: Monad z => Mode z NormalModel (Maybe CombineModel) NormalInput
normalMode =
  Mode { recognize = recognize'
       , react = react'
       }

