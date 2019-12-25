{-# LANGUAGE DeriveFunctor #-}
module Reverse.Editor.Selection
  ( Selection, select1, deselect, forward, backward, selectionOffset
  ) where

import           Data.List.AtLeast2 (AtLeast2)
import qualified Data.List.AtLeast2 as AL2
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Reverse.Editor.Contexted

data Direction = Backward | Forward

opposite :: Direction -> Direction
opposite = \case
  Backward -> Forward
  Forward -> Backward

data Selection a
  = Selected1 a
  | Selection Direction (AtLeast2 a)
  deriving Functor

select1 :: a -> Selection a
select1 = Selected1

direct :: Direction -> NonEmpty a -> NonEmpty a
direct Backward = NEL.reverse
direct Forward = id

deselect :: Selection a -> NonEmpty a
deselect (Selected1 x) = pure x
deselect (Selection d xs) = direct d $ AL2.toNonEmpty xs

redirect :: Selection a -> Selection a
redirect (Selection d xs) = Selection (opposite d) xs
redirect s = s

afterward :: (a -> f a) -> InContext a (f a) -> InContext a (f a)
afterward f c =
  case afters c of
    [] -> c
    next : newAfters -> c { current = f next, afters = newAfters }

forward :: InContext a (Selection a) -> InContext a (Selection a)
forward c =
  case current c of
    Selected1 x ->
      afterward (\next -> Selection Forward $ AL2.exactly2 next x) c
    Selection Forward xs ->
      afterward (\next -> Selection Forward $ AL2.cons next xs) c
    Selection Backward xs ->
      let (x', tail') = AL2.uncons xs
          c' = c { befores = x' : befores c }
      in case tail' of
          Left x -> c' { current = Selected1 x }
          Right xs' -> c' { current = Selection Backward xs' }

backward :: InContext a (Selection a) -> InContext a (Selection a)
backward = oneEighty . fmap redirect . forward . fmap redirect . oneEighty

polarity :: Direction -> Int
polarity = \case
  Backward -> -1
  Forward -> 1

selectionOffset :: Selection a -> Int
selectionOffset (Selected1 _) = 1
selectionOffset (Selection d xs) = polarity d * length xs
