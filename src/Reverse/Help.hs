module Reverse.Help (printHelp) where

import Data.Char (toUpper)
import Data.Foldable (traverse_)

name :: String
name = "Reverse"

tagline :: String
tagline = "a model editor for fitting free verse to a rhythm"

data ModeHelp
  = ModeHelp { modeName :: String }

type Help = [ModeHelp]

formatModeHelp :: ModeHelp -> [String]
formatModeHelp h =
  [ fmap toUpper (modeName h) <> " MODE"
  , "..."
  ]

help :: Help
help =
  [ ModeHelp { modeName = "Normal" }
  , ModeHelp { modeName = "Combine" }
  ]

format :: Help -> [String]
format h =
  [ name <> " - " <> tagline
  ] <> concatMap formatModeHelp h

printHelp :: IO ()
printHelp = traverse_ putStrLn $ format help
