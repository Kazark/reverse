module Reverse.Model (Reverse(..), derive, integrate) where

import Reverse.Base
import Reverse.UI (ViewModel(..))

-- TODO use Row here?
data Reverse = Reverse [Row String] (Row String, String, Row String) [Row String]

integrate :: (Row String, String, Row String) -> Row String
integrate (Row befores, current, Row afters) =
  Row $ reverse befores ++ [current] ++ afters

derive :: Row String -> (Row String, String, Row String)
derive (Row []) = (Row [], "", Row [])
derive (Row (x : xs)) = (Row [], x, Row xs)

instance ViewModel Reverse where
  content (Reverse befores current afters) =
    reverse befores ++ [integrate current] ++ afters
  cursorColumn (Reverse _ (befores, _, _) _) = length befores
  cursorRow (Reverse befores _ _) = length befores
