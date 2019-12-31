module Reverse.Help (printHelp) where

import           Data.Char (toUpper)
import           Data.Foldable (traverse_)
import qualified Reverse.Editor.CombineMode as Combine
import qualified Reverse.Editor.NormalMode as Normal
import           Reverse.ModeHelp

type Help = [ModeHelp]

name :: String
name = "Reverse"

tagline :: String
tagline = "a model editor for fitting free verse to a rhythm"

formatModeHelp :: ModeHelp -> [String]
formatModeHelp h =
  fmap toUpper (modeName h) <> " MODE"
  : describe h [' '..'~']

help :: Help
help =
  [ Normal.help
  , Combine.help
  ]

format :: Help -> [String]
format h =
  [ name <> " - " <> tagline
  ] <> concatMap formatModeHelp h

printHelp :: IO ()
printHelp = traverse_ putStrLn $ format help
