{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reverse.Editor.Model
  ( Cell(..), Row(..), Model, NormalModel, CombineModel
  , normal, combineWith
  ) where

import Data.List (intercalate)
import Reverse.Editor.Contexted
import Reverse.Editor.Delimiter (Delim, delimToChar)
import Reverse.Editor.Selection (Selection)

data Cell
  = Cell { accented :: Bool
         , cellContents :: String
         }

normal :: String -> Cell
normal = Cell False

combineWith :: Delim -> [Cell] -> Cell
combineWith d cs =
  Cell { accented = all accented cs
       , cellContents = intercalate [delimToChar d] $ fmap cellContents cs
       }

instance EmptyFocus Cell where
  emptyFocus = normal ""

-- We don't focus within a cell (yet)
instance Focused Cell where
  focusedOn _ = 0

newtype Row a
  = Row [a]
  deriving Functor
  deriving Semigroup via [a]
  deriving Monoid via [a]
  deriving Foldable via []

instance Blur (InContext Cell Cell) (Row Cell) where
  blur = Row . blur

instance Contexted (InContext Cell Cell) (Row Cell) where
  focusOn i (Row r) = focusOn i r

type Model a = InContext (Row Cell) (InContext Cell a)
type NormalModel = Model Cell
type CombineModel = Model (Selection Cell)
