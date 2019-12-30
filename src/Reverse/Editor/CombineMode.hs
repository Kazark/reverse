module Reverse.Editor.CombineMode
  ( CombineInput, combineMode, cancel
  ) where

import qualified Data.List.NonEmpty as NEL
import           Reverse.Editor.Contexted
import           Reverse.Editor.Delimiter
import           Reverse.Editor.Mode
import           Reverse.Editor.Model
import           Reverse.Editor.Selection

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
react' Cancel model = Left $ cancel model
react' (Unsplit d) model = Left $ fmap (unsplit d) model
react' ExtendSelectionLeft model = Right $ fmap backward model
react' ExtendSelectionRight model = Right $ fmap forward model

combineMode :: Monad z => Mode z CombineModel NormalModel CombineInput
combineMode =
  Mode { recognize = recognize'
       , react = react'
       }
