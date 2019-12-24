{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reverse.Contexted
  ( InContext(..), EmptyFocus(..), Blur(..), Focused(..), Contexted(..)
  , oneEighty, after, before
  ) where

import Data.Bifunctor (Bifunctor(..))

data InContext a b
  = InContext { befores :: [a]
              , current :: b
              , afters :: [a]
              }

oneEighty :: InContext a b -> InContext a b
oneEighty c = c { befores = afters c, afters = befores c }

-- | Move towards the afters
after :: InContext a a -> InContext a a
after c =
  case afters c of
    [] -> c
    newFocus : newAfters ->
      InContext (current c : befores c) newFocus newAfters

-- | Move towards the befores
before :: InContext a a -> InContext a a
before = oneEighty . after . oneEighty

instance Bifunctor InContext where
  bimap f g ctxt =
    InContext (f <$> befores ctxt) (g $ current ctxt) (f <$> afters ctxt)

instance Functor (InContext a) where
  fmap = second

class EmptyFocus a where
  emptyFocus :: a

-- | Lose focus on something focused in a context
class Blur a b where
  blur :: a -> b

instance Blur a a where
  blur = id

instance Blur a b => Blur (InContext b a) [b] where
  blur c = reverse (befores c) <> [blur $ current c] <> afters c

class Focused a where
  focusedOn :: a -> Int

instance Focused (InContext b a) where
  focusedOn (InContext b _ _) = length b

class Blur a b => Contexted a b where
  focusOn :: Int -> b -> a

instance Contexted a a where
  focusOn _ = id

instance EmptyFocus a => Contexted (InContext a a) [a] where
  focusOn = focusOn' [] where
    focusOn' :: EmptyFocus a => [a] -> Int -> [a] -> InContext a a
    focusOn' b _ [] = InContext b emptyFocus []
    focusOn' b _ [x] = InContext b x []
    focusOn' b 0 (x : xs) = InContext b x xs
    focusOn' b i (x : xs) = focusOn' (x : b) (i - 1) xs

