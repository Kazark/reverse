module Reverse.Model (Reverse(..), derive, integrate, ingest) where

import Reverse.Base
import Reverse.UI (ViewModel(..))

data Reverse
  = Reverse [Row String] (Row String, String, Row String) [Row String]

ingest :: String -> Reverse
ingest text =
  let (current, rest) =
        case lines text of
          [] -> ("", [])
          x : xs -> (x, xs)
  in Reverse [] (Row [], current, Row []) $ fmap (Row . pure) rest

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
