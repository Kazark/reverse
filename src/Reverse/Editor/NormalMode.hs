module Reverse.Editor.NormalMode
  ( NormalInput, normalMode, ingest
  ) where

import Data.List.Split (wordsBy)
import Reverse.Editor.Contexted
import Reverse.Editor.Mode
import Reverse.Editor.Model
import Reverse.Editor.Selection (select1)

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

normalActionMap :: [(Char, NormalInput)]
normalActionMap =
  [ ('q',  Quit)
  , ('c',  Combine)
  , ('k',  Act MoveUp)
  , ('j',  Act MoveDown)
  , ('h',  Act MoveLeft)
  , ('l',  Act MoveRight)
  , ('\'', Act Accent)
  , ('s',  Act Split)
  , ('1',  Act $ Divide 1)
  , ('2',  Act $ Divide 2)
  , ('3',  Act $ Divide 3)
  , ('4',  Act $ Divide 4)
  , ('5',  Act $ Divide 5)
  , ('6',  Act $ Divide 6)
  , ('7',  Act $ Divide 7)
  , ('8',  Act $ Divide 8)
  , ('9',  Act $ Divide 9)
  ]

inputHelp :: NormalInput -> String
inputHelp = \case
  Quit -> "quit the program without saving"
  Combine -> "switch into combine mode"
  Act MoveUp -> "move the cursor up a line"
  Act MoveDown -> "move the cursor down a line"
  Act MoveLeft -> "move the cursor to the left"
  Act MoveRight -> "move the cursor to the right"
  Act Accent -> "toggle accent on the cell"
  Act Split -> "split the (previously combined) cell"
  Act (Divide x) -> "divide out a syllable " ++ show x ++ " characters long"

normalMode :: Mode NormalModel (Maybe CombineModel) NormalInput
normalMode =
  Mode { modeName = "normal"
       , actionMap = normalActionMap
       , react = react'
       , actionHelp = inputHelp
       }
