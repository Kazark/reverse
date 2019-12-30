module Reverse.Help (help) where

import Data.Foldable (traverse_)

help :: IO ()
help = traverse_ putStrLn ["NORMAL Mode", "...", "COMBINE MODE", "..."]
