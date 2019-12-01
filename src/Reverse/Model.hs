module Reverse.Model
  ( Reverse(..), derive, integrate, ingest, cursorIndex, splitOn
  ) where

import Data.List.Split (wordsBy)
import Reverse.Base
import Reverse.UI (ViewModel(..))

data Reverse
  = Reverse [Row Cell] (Row Cell, Cell, Row Cell) [Row Cell]

ingest :: String -> Reverse
ingest text =
  let (current, afters) =
        case fmap (splitOn ' ' . normal) $ lines text of
          [] -> ([normal ""], [])
          x : xs -> (x, xs)
  in Reverse [] (derive 0 $ Row current) $ fmap Row afters

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

splitOn :: Char -> Cell -> [Cell]
splitOn c current =
  let mkCell cc = current { cellContents = cc }
  in fmap mkCell $ wordsBy (== c) $ cellContents current

instance ViewModel Reverse where
  content (Reverse befores current afters) =
    reverse befores ++ [integrate current] ++ afters
  cursorColumn (Reverse _ current _) = cursorIndex current
  cursorRow (Reverse befores _ _) = length befores
