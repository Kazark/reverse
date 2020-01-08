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
  | DecreaseSelection
  | IncreaseSelection
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
  [ ('h', DecreaseSelection)
  , ('l', IncreaseSelection)
  , ('q', Cancel)
  , ('c', Combine)
  ]

react' :: CombineInput -> CombineModel -> Either NormalModel CombineModel
react' = \case
  Cancel -> Left . cancel
  Combine -> Left . fmap (fmap (combine . NEL.toList . deselect))
  DecreaseSelection -> Right . fmap backward
  IncreaseSelection -> Right . fmap forward

inputHelp :: CombineInput -> String
inputHelp = \case
  Combine -> "combine the current selection"
  DecreaseSelection -> "decrease the selection (to the left)"
  IncreaseSelection -> "increase the selection (to the right)"
  Cancel -> "cancel out of combine mode/switch back to normal mode"

combineMode :: Mode CombineModel NormalModel CombineInput
combineMode =
  Mode { modeName = "combine"
       , actionMap = combineActionMap
       , react = react'
       , actionHelp = inputHelp
       }
