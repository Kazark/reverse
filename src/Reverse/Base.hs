{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
module Reverse.Base (Row(..)) where

newtype Row a
  = Row [a]
  deriving Functor
  deriving Semigroup via [a]
  deriving Monoid via [a]
