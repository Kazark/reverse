{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
module Reverse.Base (Cell(..), Row(..), normal) where

data Cell
  = Cell { accented :: Bool
         , cellContents :: String
         }

normal :: String -> Cell
normal = Cell False

newtype Row a
  = Row [a]
  deriving Functor
  deriving Semigroup via [a]
  deriving Monoid via [a]
  deriving Foldable via []
