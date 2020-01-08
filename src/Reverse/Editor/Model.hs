{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reverse.Editor.Model
  ( Cell, Row(..), Model, NormalModel, CombineModel
  , normal, combine, toggleAccent, setCellContents, getCellContents
  , isAccented, splitOffPrefixAt, splitCell
  ) where

import Reverse.Editor.Contexted
import Reverse.Editor.Selection (Selection)

data Subcell
  = Prefix String
  | Chunk String

subcellContents :: Subcell -> String
subcellContents = \case
  Prefix s ->  s
  Chunk s -> s

replaceInSubcell :: Subcell -> String -> Subcell
replaceInSubcell = \case
  Prefix _ ->  Prefix
  Chunk _ -> Chunk

splitSubcellAt :: Int -> Subcell -> [Subcell]
splitSubcellAt x sc =
  let (prefix, suffix) = splitAt x $ subcellContents sc
  in [Prefix prefix, replaceInSubcell sc suffix]

instance Show Subcell where
  show = \case
    Prefix s -> s ++ "-"
    Chunk s -> s

data Cell
  = Cell { accented :: Bool
         , cellContents :: [Subcell]
         }

emptyCell :: Cell
emptyCell = Cell { accented = False
                 , cellContents = []
                 }

setCellContents :: Cell -> String -> Cell
setCellContents c cc = c { cellContents = [Chunk cc] }

getCellContents :: Cell -> String
getCellContents = unwords . fmap show . cellContents

toggleAccent :: Cell -> Cell
toggleAccent c = c { accented = not (accented c) }

normal :: String -> Cell
normal = setCellContents emptyCell

isAccented :: Cell -> Bool
isAccented = accented

combine :: [Cell] -> Cell
combine cs =
  Cell { accented = all accented cs
       , cellContents = concatMap cellContents cs
       }

splitOffPrefixAt :: Int -> Cell -> [Cell]
splitOffPrefixAt x c@Cell { cellContents = [sc] } =
  (\cc -> c { cellContents = [cc] }) <$> splitSubcellAt x sc
splitOffPrefixAt _ c = pure c

splitCell :: Cell -> [Cell]
splitCell curCell =
  (\cc -> curCell { cellContents = [cc] }) <$> cellContents curCell

instance EmptyFocus Cell where
  emptyFocus = normal ""

-- We don't focus within a cell
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
