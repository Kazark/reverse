{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.List.AtLeast2
  ( atLeast2, AtLeast2(..), toNonEmpty, toList, exactly2, cons, uncons
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL

infixr 5 ::| -- same as :|

data AtLeast2 a
  = a ::| NonEmpty a
  deriving (Eq, Show, Functor, Foldable, Traversable)

atLeast2 :: [a] -> Maybe (AtLeast2 a)
atLeast2 (x : x' : xs) = Just $ x ::| x' :| xs
atLeast2 _ = Nothing

toNonEmpty :: AtLeast2 a -> NonEmpty a
toNonEmpty (x ::| xs) = x :| NEL.toList xs

toList :: AtLeast2 a -> [a]
toList = NEL.toList . toNonEmpty

exactly2 :: a -> a -> AtLeast2 a
exactly2 x y = x ::| y :| []

cons :: a -> AtLeast2 a -> AtLeast2 a
cons x (x' ::| xs) = x ::| NEL.cons x' xs

uncons :: AtLeast2 a -> (a, Either a (AtLeast2 a))
uncons (x ::| xs) = (x, tail') where
  tail' =
    case NEL.uncons xs of
      (x', Nothing) -> Left x'
      (x', Just xs') -> Right (x' ::| xs')
