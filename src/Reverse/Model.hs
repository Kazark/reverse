{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reverse.Model
  ( Cell(..), Row(..), Model
  , normal
  ) where

import Reverse.Contexted

data Cell
  = Cell { accented :: Bool
         , cellContents :: String
         }

normal :: String -> Cell
normal = Cell False

instance EmptyFocus Cell where
  emptyFocus = normal ""

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
