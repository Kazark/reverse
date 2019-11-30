module Reverse.Model (Reverse(..), derive, integrate) where

import Reverse.UI (Outputtable(..), Row(..))

-- TODO use Row here?
data Reverse = Reverse [[String]] ([String], String, [String]) [[String]]

integrate :: ([String], String, [String]) -> [String]
integrate (befores, current, afters) = reverse befores ++ [current] ++ afters

derive :: [String] -> ([String], String, [String])
derive [] = ([], "", [])
derive (x : xs) = ([], x, xs)

instance Outputtable Reverse where
  output (Reverse befores current afters) =
    fmap Row $ befores ++ [integrate current] ++ afters
