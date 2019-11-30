module Reverse.UI (ViewModel(..), TermEnv, initTerm, redraw) where

import Reverse.Base
import System.Console.Terminfo
import System.Exit (die)
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf (FieldFormat(..), FormatAdjustment(..), formatString)

class ViewModel a where
  content :: a -> [Row String]
  cursorColumn :: a -> Int
  cursorRow :: a -> Int

indexOr0 :: Int -> Row Int -> Int
indexOr0 i (Row xs) =
  if i < length xs
  then xs !! i
  else 0

maxColumns :: [Row a] -> Int
maxColumns = maximum . fmap length

colWidths :: [Row String] -> [Int]
colWidths x =
  let ns = fmap (fmap length) x
      lastCol = maxColumns x-1
      colSizes = fmap (\i -> fmap (indexOr0 i) ns) [0..lastCol]
  in fmap maximum colSizes

markWidths :: [Int] -> Row String -> Row (Int, String)
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

padRow :: Row (Int, String) -> Row String
padRow = fmap (\(i, s) -> formatString s (padBy i) "")

renderRow :: Row String -> String
renderRow (Row xs) = unwords xs

cursorPosition :: ViewModel a => a -> [Int] -> Point
cursorPosition v cols =
  Point { row = cursorRow v
        , col = sum $ fmap (+ 1) $ take (cursorColumn v) cols
        }

layout :: [Int] -> [Row String] -> TermOutput
layout widths =
  termText . unlines . fmap (renderRow . padRow . markWidths widths)

data TermEnv
  = TermEnv { term :: Terminal
            , cls :: LinesAffected -> TermOutput
            , moveCursorTo :: Point -> TermOutput
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
  return $ TermEnv { term = term'
                   , cls = clearS
                   , moveCursorTo = moveC
                   }

redraw :: ViewModel a => TermEnv -> a -> IO ()
redraw env x =
  let whatShouldThisBe = 1337
      ctnt = content x
      widths = colWidths ctnt
      blankSlate = cls env whatShouldThisBe
      newContent = layout widths ctnt
      cursor = moveCursorTo env $ cursorPosition x widths
  in runTermOutput (term env) $ blankSlate <> newContent <> cursor
