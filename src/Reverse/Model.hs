module Reverse.Model
  ( Reverse(..), derive, integrate, ingest, cursorIndex
  ) where

import Reverse.Base
import Reverse.UI (ViewModel(..))

data Reverse
  = Reverse [Row Cell] (Row Cell, Cell, Row Cell) [Row Cell]

ingest :: String -> Reverse
ingest text =
  let (current, rest) =
        case lines text of
          [] -> ("", [])
          x : xs -> (x, xs)
      afters = fmap (Row . pure . normal) rest
  in Reverse [] (Row [], normal current, Row []) afters

integrate :: (Row Cell, Cell, Row Cell) -> Row Cell
integrate (Row befores, current, Row afters) =
  Row $ reverse befores ++ [current] ++ afters

softByIndex :: Int -> [Cell] -> ([Cell], Cell, [Cell])
softByIndex = softByIndex' [] where
  softByIndex' :: [Cell] -> Int -> [Cell] -> ([Cell], Cell, [Cell])
  softByIndex' befores _ [] = (befores, normal "", [])
  softByIndex' befores _ [x] = (befores, x, [])
  softByIndex' befores 0 (x : xs) = (befores, x, xs)
  softByIndex' befores i (x : xs) = softByIndex' (x : befores) (i - 1) xs

derive :: Int -> Row Cell -> (Row Cell, Cell, Row Cell)
derive i (Row xs) =
  let (befores, current, after) = softByIndex i xs
  in (Row befores, current, Row after)

cursorIndex :: (Row Cell, Cell, Row Cell) -> Int
cursorIndex (befores, _, _) = length befores

instance ViewModel Reverse where
  content (Reverse befores current afters) =
    reverse befores ++ [integrate current] ++ afters
  cursorColumn (Reverse _ current _) = cursorIndex current
  cursorRow (Reverse befores _ _) = length befores
