module Reverse.UI
  ( ViewModel(..), TermEnv
  , initTerm, redraw, resetScreen
  ) where

import Data.Function ((&))
import Data.List (intersperse)
import Reverse.Editor.Model (Row(..), Cell, getCellContents, isAccented)
import System.Console.Terminfo
import System.Exit (die)
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf (FieldFormat(..), FormatAdjustment(..), formatString)

data UICell =
  UICell { cell :: Cell
         , selected :: Bool
         }

data ViewModel = ViewModel
  { content :: [Row Cell]
  , cursorColumn :: Int
  , cursorRow :: Int
  , selection :: Maybe (Int, Int)
  }

indexOr0 :: Int -> Row Int -> Int
indexOr0 i (Row xs) =
  if i < length xs
  then xs !! i
  else 0

maxColumns :: [Row a] -> Int
maxColumns = maximum . fmap length

colWidths :: [Row Cell] -> [Int]
colWidths x =
  let ns = fmap (fmap (length . getCellContents)) x
      lastCol = maxColumns x-1
      colSizes = fmap (\i -> fmap (indexOr0 i) ns) [0..lastCol]
  in fmap maximum colSizes

markWidths :: [Int] -> Row UICell -> Row (Int, UICell)
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
maybeAccent env c =
  if isAccented c
  then embolden env
  else id

maybeInvert :: TermEnv -> UICell -> TermOutput -> TermOutput
maybeInvert env c =
  if selected c
  then markSelected env
  else id

padRow :: TermEnv -> Row (Int, UICell) -> Row TermOutput
padRow env =
  fmap \(i, c) ->
    formatString (getCellContents $ cell c) (padBy i) ""
    & termText
    & maybeAccent env (cell c)
    & maybeInvert env c

uncolumns :: [TermOutput] -> TermOutput
uncolumns = mconcat . intersperse (termText " ")

renderRow :: Row TermOutput -> TermOutput
renderRow (Row xs) = uncolumns xs

columnBeginsAt :: [Int] -> Int -> Int
columnBeginsAt cols colN = sum $ fmap (+ 1) $ take colN cols

cursorPosition :: ViewModel -> [Int] -> Point
cursorPosition v cols =
  Point { row = cursorRow v
        , col = columnBeginsAt cols $ cursorColumn v
        }

unrows :: [TermOutput] -> TermOutput
unrows = mconcat . intersperse (termText "\n")

layout :: TermEnv -> [Int] -> [Row UICell] -> TermOutput
layout env widths =
  unrows . fmap (renderRow . padRow env . markWidths widths)

uiCells :: ViewModel -> [Row UICell]
uiCells vm = do
  let befores = take (cursorRow vm) $ repeat Nothing
  let maybeSels = befores <> [selection vm] <> repeat Nothing
  (r, maybeSel) <- zip (content vm) maybeSels
  return $ uiRow r maybeSel
  where
    uiRow r Nothing = fmap (\c -> UICell c False) r
    uiRow (Row r) (Just (from, offset)) = Row do
      let befores' = take from $ repeat False
      let sel = take offset $ repeat True
      let sels = befores' <> sel <> repeat False
      (c, s) <- zip r sels
      return $ UICell c s

data TermEnv
  = TermEnv { term :: Terminal
            , cls :: LinesAffected -> TermOutput
            , moveCursorTo :: Point -> TermOutput
            , embolden :: TermOutput -> TermOutput
            , markSelected :: TermOutput -> TermOutput
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
  markS <- requireCapability term' withUnderline
  return $ TermEnv { term = term'
                   , cls = clearS
                   , moveCursorTo = moveC
                   , embolden = withB
                   , markSelected = markS
                   }

clear :: TermEnv -> TermOutput
clear env = cls env 1337 -- What should this number actually be?

redraw :: TermEnv -> ViewModel -> IO ()
redraw env x =
  let widths = colWidths $ content x
      blankSlate = clear env
      newContent = layout env widths $ uiCells x
      cursor = moveCursorTo env $ cursorPosition x widths
  in runTermOutput (term env) $ blankSlate <> newContent <> cursor

resetScreen :: TermEnv -> IO ()
resetScreen env = runTermOutput (term env) $ clear env
