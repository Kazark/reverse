{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Reverse.Model
  ( Normal, Contexted(..)
  , derive, integrate, ingest, cursorIndex, splitOn
  ) where

import Data.List.Split (wordsBy)
import Reverse.Base
import Reverse.UI (ViewModel(..))
import Data.Bifunctor (Bifunctor(..))

data Contexted a b
  = Contexted { befores :: a
              , focus :: b
              , afters :: a
              }

instance Bifunctor Contexted where
  bimap f g ctxt =
    Contexted (f $ befores ctxt) (g $ focus ctxt) (f $ afters ctxt)

type Normal = Contexted [Row Cell] (Contexted (Row Cell) Cell)

instance ViewModel Normal where
  content (Contexted b current a) =
    reverse b ++ [integrate current] ++ a
  cursorColumn (Contexted _ current _) = cursorIndex current
  cursorRow (Contexted b _ _) = length b

ingest :: String -> Normal
ingest text =
  let (current, a) =
        case fmap (splitOn ' ' . normal) $ lines text of
          [] -> ([normal ""], [])
          x : xs -> (x, xs)
  in Contexted { befores = []
               , focus = derive 0 $ Row current
               , afters = fmap Row a
               }

integrate :: Contexted (Row Cell) Cell -> Row Cell
integrate (Contexted (Row b) current (Row a)) =
  Row $ reverse b ++ [current] ++ a

softByIndex :: Int -> [Cell] -> Contexted [Cell] Cell
softByIndex = softByIndex' [] where
  softByIndex' :: [Cell] -> Int -> [Cell] -> Contexted [Cell] Cell
  softByIndex' b _ [] = Contexted b (normal "") []
  softByIndex' b _ [x] = Contexted b x []
  softByIndex' b 0 (x : xs) = Contexted b x xs
  softByIndex' b i (x : xs) = softByIndex' (x : b) (i - 1) xs

derive :: Int -> Row Cell -> Contexted (Row Cell) Cell
derive i (Row xs) = first Row $ softByIndex i xs

cursorIndex :: Contexted (Row Cell) Cell -> Int
cursorIndex (Contexted b _ _) = length b

splitOn :: Char -> Cell -> [Cell]
splitOn c current =
  let mkCell cc = current { cellContents = cc }
  in fmap mkCell $ wordsBy (== c) $ cellContents current
