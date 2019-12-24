{-# LANGUAGE FlexibleInstances #-}
module Reverse.UI
  ( ViewModel(..), TermEnv, initTerm, redraw, resetScreen
  ) where

import qualified Data.List.NonEmpty as NEL
import Data.List (intersperse)
import Reverse.Contexted
import Reverse.Model (Row(..), Cell(..))
import System.Console.Terminfo
import System.Exit (die)
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf (FieldFormat(..), FormatAdjustment(..), formatString)

class ViewModel a where
  content :: a -> [Row Cell]
  cursorColumn :: a -> Int
  cursorRow :: a -> Int

instance ViewModel (InContext (Row Cell) (InContext Cell Cell)) where
  content = blur
  cursorColumn = focusedOn . current
  cursorRow = length . befores

instance ViewModel (InContext (Row Cell) (InContext Cell (NEL.NonEmpty Cell))) where
  content =
    blur . fmap (\c ->
      c { current = NEL.head $ current c
        , afters = NEL.tail (current c) <> afters c
        }
    )
  cursorColumn = focusedOn . current
  cursorRow = length . befores

indexOr0 :: Int -> Row Int -> Int
indexOr0 i (Row xs) =
  if i < length xs
  then xs !! i
  else 0

maxColumns :: [Row a] -> Int
maxColumns = maximum . fmap length

colWidths :: [Row Cell] -> [Int]
colWidths x =
  let ns = fmap (fmap (length . cellContents)) x
      lastCol = maxColumns x-1
      colSizes = fmap (\i -> fmap (indexOr0 i) ns) [0..lastCol]
  in fmap maximum colSizes

markWidths :: [Int] -> Row Cell -> Row (Int, Cell)
markWidths is (Row ss) = Row $ zip is ss

padBy :: Int -> FieldFormat
padBy x =
  FieldFormat { fmtWidth = Just x
              , fmtPrecision = Nothing
              , fmtAdjust = Just LeftAdjust
              , fmtSign = Nothing
              , fmtAlternate = False
              , fmtModifiers = ""
              , fmtChar = 's'
              }

maybeAccent :: TermEnv -> Cell -> TermOutput -> TermOutput
maybeAccent env cell =
  if accented cell
  then embolden env
  else id

padRow :: TermEnv -> Row (Int, Cell) -> Row TermOutput
padRow env = fmap \(i, cell) ->
  let renderedContents = formatString (cellContents cell) (padBy i) ""
  in maybeAccent env cell $ termText renderedContents

uncolumns :: [TermOutput] -> TermOutput
uncolumns = mconcat . intersperse (termText " ")

renderRow :: Row TermOutput -> TermOutput
renderRow (Row xs) = uncolumns xs

cursorPosition :: ViewModel a => a -> [Int] -> Point
cursorPosition v cols =
  Point { row = cursorRow v
        , col = sum $ fmap (+ 1) $ take (cursorColumn v) cols
        }

unrows :: [TermOutput] -> TermOutput
unrows = mconcat . intersperse (termText "\n")

layout :: TermEnv -> [Int] -> [Row Cell] -> TermOutput
layout env widths =
  unrows . fmap (renderRow . padRow env . markWidths widths)

data TermEnv
  = TermEnv { term :: Terminal
            , cls :: LinesAffected -> TermOutput
            , moveCursorTo :: Point -> TermOutput
            , embolden :: TermOutput -> TermOutput
            }

requireCapability :: Terminal -> Capability a -> IO a
requireCapability t cap =
  case getCapability t cap of
    Nothing -> die "Your terminal doesn't support the required capabilities."
    Just x -> return x

initTerm :: IO TermEnv
initTerm = do
  hSetBuffering stdin NoBuffering
  term' <- setupTermFromEnv
  clearS <- requireCapability term' clearScreen
  moveC <- requireCapability term' cursorAddress
  withB <- requireCapability term' withBold
  return $ TermEnv { term = term'
                   , cls = clearS
                   , moveCursorTo = moveC
                   , embolden = withB
                   }

clear :: TermEnv -> TermOutput
clear env = cls env 1337 -- What should this number actually be?

redraw :: ViewModel a => TermEnv -> a -> IO ()
redraw env x =
  let ctnt = content x
      widths = colWidths ctnt
      blankSlate = clear env
      newContent = layout env widths ctnt
      cursor = moveCursorTo env $ cursorPosition x widths
  in runTermOutput (term env) $ blankSlate <> newContent <> cursor

resetScreen :: TermEnv -> IO ()
resetScreen env = runTermOutput (term env) $ clear env
