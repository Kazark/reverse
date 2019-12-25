module Reverse.Editor.CombineMode ( CombineInput, combineMode ) where

import qualified Data.List.NonEmpty as NEL
import           Reverse.Editor.Contexted
import           Reverse.Editor.Delimiter
import           Reverse.Editor.Mode
import           Reverse.Editor.Model

data CombineInput
  = Unsplit Delim
  | ExtendSelectionLeft
  | ExtendSelectionRight
  | Cancel

cancel :: CombineModel -> NormalModel
cancel =
  fmap (\c ->
    c { current = NEL.head $ current c
      , afters = NEL.tail (current c) <> afters c
      }
  )

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
react' _ {- TODO -} model = Right model

combineMode :: Monad z => Mode z CombineModel NormalModel CombineInput
combineMode =
  Mode { recognize = recognize'
       , react = react'
       }
