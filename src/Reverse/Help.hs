module Reverse.Help (printHelp) where

import Text.Printf (printf)
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Reverse.Editor.Mode
import Reverse.Editor.CombineMode (combineMode)
import Reverse.Editor.NormalMode (normalMode)

name :: String
name = "Reverse"

tagline :: String
tagline = "a modal editor for fitting free verse to a rhythm"

describe :: Char -> String -> String
describe = printf "%c - %s"

formatModeHelp :: Mode s o a -> [String]
formatModeHelp h =
  fmap toUpper (modeName h) <> " MODE"
  : fmap (uncurry describe . fmap (actionHelp h)) (actionMap h)

help :: [String]
help =
  concat [ [name <> " - " <> tagline]
         , "" : formatModeHelp normalMode
         , "" : formatModeHelp combineMode
         ]

printHelp :: IO ()
printHelp = traverse_ putStrLn help
