{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reverse.Editor.Model
  ( Cell(..), Row(..), Model, NormalModel, CombineModel
  , normal
  ) where

import Data.List.NonEmpty (NonEmpty)
import Reverse.Editor.Contexted

data Cell
  = Cell { accented :: Bool
         , cellContents :: String
         }

normal :: String -> Cell
normal = Cell False

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
type CombineModel = Model (NonEmpty Cell)
