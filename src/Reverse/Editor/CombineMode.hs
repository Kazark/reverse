module Reverse.Editor.CombineMode
  ( CombineInput, combineMode, cancel
  ) where

import qualified Data.List.NonEmpty as NEL
import           Reverse.Editor.Contexted
import           Reverse.Editor.Mode
import           Reverse.Editor.Model
import           Reverse.Editor.Selection

data CombineInput
  = Combine
  | ExtendSelectionLeft
  | ExtendSelectionRight
  | Cancel

cancel' :: InContext a (Selection a) -> InContext a a
cancel' c =
  let xs = deselect $ current c
  in c { current = NEL.head xs
       , afters = NEL.tail xs <> afters c
       }

cancel :: CombineModel -> NormalModel
cancel = fmap cancel'

combineActionMap :: [(Char, CombineInput)]
combineActionMap =
  [ ('h', ExtendSelectionLeft)
  , ('l', ExtendSelectionRight)
  , ('q', Cancel)
  , ('c', Combine)
  ]

react' :: CombineInput -> CombineModel -> Either NormalModel CombineModel
react' = \case
  Cancel -> Left . cancel
  Combine -> Left . fmap (fmap (combine . NEL.toList . deselect))
  ExtendSelectionLeft -> Right . fmap backward
  ExtendSelectionRight -> Right . fmap forward

inputHelp :: CombineInput -> String
inputHelp _ = "..."

combineMode :: Mode CombineModel NormalModel CombineInput
combineMode =
  Mode { modeName = "combine"
       , actionMap = combineActionMap
       , react = react'
       , actionHelp = inputHelp
       }
