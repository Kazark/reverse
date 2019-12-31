module Reverse.Editor.CombineMode
  ( CombineInput, combineMode, cancel, help
  ) where

import qualified Data.List.NonEmpty as NEL
import           Reverse.Editor.Contexted
import           Reverse.Editor.Delimiter
import           Reverse.Editor.Mode
import           Reverse.Editor.Model
import           Reverse.Editor.Selection
import           Reverse.ModeHelp

data CombineInput
  = Unsplit Delim
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

unsplit :: Delim -> InContext Cell (Selection Cell) -> InContext Cell Cell
unsplit d c = c { current = combineWith d $ NEL.toList $ deselect $ current c }

recognize' :: Monad z => z Char -> z CombineInput
recognize' getChr = do
  c <- getChr
  case c of
    'h'  -> return ExtendSelectionLeft
    'l'  -> return ExtendSelectionRight
    'q'  -> return Cancel
    _    ->
      case charToDelim c of
        Just d -> return $ Unsplit d
        Nothing -> recognize' getChr

react' :: CombineInput -> CombineModel -> Either NormalModel CombineModel
react' = \case
  Cancel -> Left . cancel
  (Unsplit d) -> Left . fmap (unsplit d)
  ExtendSelectionLeft -> Right . fmap backward
  ExtendSelectionRight -> Right . fmap forward

combineMode :: Monad z => Mode z CombineModel NormalModel CombineInput
combineMode =
  Mode { recognize = recognize'
       , react = react'
       }

help :: ModeHelp
help =
  ModeHelp { modeName = "combine"
           , describe = fmap inputHelp . recognize'
           } where
  inputHelp :: CombineInput -> String
  inputHelp _ = "..."
