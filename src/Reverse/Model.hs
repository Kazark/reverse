module Reverse.Model (Reverse(..), derive, integrate) where

import Reverse.Base
import Reverse.UI (Outputtable(..))

-- TODO use Row here?
data Reverse = Reverse [Row String] (Row String, String, Row String) [Row String]

integrate :: (Row String, String, Row String) -> Row String
integrate (Row befores, current, Row afters) =
  Row $ reverse befores ++ [current] ++ afters

derive :: Row String -> (Row String, String, Row String)
derive (Row []) = (Row [], "", Row [])
derive (Row (x : xs)) = (Row [], x, Row xs)

instance Outputtable Reverse where
  output (Reverse befores current afters) =
    reverse befores ++ [integrate current] ++ afters
