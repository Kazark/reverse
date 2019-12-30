module Reverse.Editor.Selection
  ( Selection, select1, deselect, forward, backward, selectionOffset
  ) where

import           Data.List.AtLeast2 (AtLeast2(..))
import qualified Data.List.AtLeast2 as AL2
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Reverse.Editor.Contexted

newtype Selection a = S (NonEmpty a)

select1 :: a -> Selection a
select1 = S . pure

deselect :: Selection a -> NonEmpty a
deselect (S x) = x

extend :: a -> Selection a -> Selection a
extend x (S xs) = S $ NEL.cons x xs

forward :: InContext a (Selection a) -> InContext a (Selection a)
forward c =
  case afters c of
    [] -> c
    next : newAfters ->
      c { current = extend next $ current c, afters = newAfters }

backward :: InContext a (Selection a) -> InContext a (Selection a)
backward c =
  let (S c') = current c in
  case AL2.ofNonEmpty c' of
    Nothing -> c
    Just (x ::| xs) -> c { current = S xs, afters = x : afters c}

selectionOffset :: Selection a -> Int
selectionOffset (S x) = length x
