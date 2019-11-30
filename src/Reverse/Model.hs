module Reverse.Model (Reverse(..), derive, integrate) where

import Reverse.UI (Outputtable(..))
import System.Console.Terminfo (termText)

data Reverse = Reverse [[String]] ([String], String, [String]) [[String]]

integrate :: ([String], String, [String]) -> [String]
integrate (befores, current, afters) = reverse befores ++ [current] ++ afters

derive :: [String] -> ([String], String, [String])
derive [] = ([], "", [])
derive (x : xs) = ([], x, xs)

flatten :: Reverse -> [String]
flatten (Reverse befores current afters) =
  fmap unwords (befores ++ [integrate current] ++ afters)

instance Outputtable Reverse where
  output = termText . unlines . flatten
