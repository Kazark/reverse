module Reverse.Editor.NormalMode
  ( NormalInput, normalMode, ingest, help
  ) where

import Text.Read (readMaybe)
import Data.List.Split (wordsBy)
import Reverse.Editor.Contexted
import Reverse.Editor.Mode
import Reverse.Editor.Model
import Reverse.Editor.Selection (select1)
import Reverse.ModeHelp

data Action
  = Split
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Accent
  | Divide Int

data NormalInput
  = Act Action
  | Combine
  | Quit

splitOn :: Char -> Cell -> [Cell]
splitOn chr curCell =
  fmap (setCellContents curCell) $ wordsBy (== chr) $ getCellContents curCell

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

splitWith :: (Cell -> [Cell]) -> InContext Cell Cell -> InContext Cell Cell
splitWith f ctxt =
  let (s, ss) = case f (current ctxt) of
                  [] -> (current ctxt, [])
                  x : xs -> (x, xs)
  in ctxt { current = s, afters = ss <> afters ctxt }

act :: Action -> NormalModel -> NormalModel
act Accent = fmap $ fmap toggleAccent
act Split = fmap $ splitWith splitCell
act MoveUp = before
act MoveDown = after
act MoveLeft = fmap before
act MoveRight = fmap after
act (Divide at) = fmap $ splitWith $ splitOffPrefixAt at

initCombine :: NormalModel -> CombineModel
initCombine = fmap (fmap select1)

react' :: NormalInput -> NormalModel
       -> Either (Maybe CombineModel) NormalModel
react' Quit _ = Left Nothing
react' Combine model = Left $ Just $ initCombine model
react' (Act action) model = Right $ act action model

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
    's'  -> return $ Act Split
    _    ->
      case readMaybe [c] of
        Just x -> return $ Act $ Divide x
        Nothing -> recognize' getChr

normalMode :: Monad z => Mode z NormalModel (Maybe CombineModel) NormalInput
normalMode =
  Mode { recognize = recognize'
       , react = react'
       }

help :: ModeHelp
help =
  ModeHelp { modeName = "normal"
           , describe = fmap inputHelp . recognize'
           } where
  inputHelp :: NormalInput -> String
  inputHelp _ = "..."
