module Reverse.UI (Outputtable(..), TermEnv, initTerm, redraw) where

import System.Console.Terminfo
import System.Exit (die)
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering))

class Outputtable a where
  output :: a -> TermOutput

data TermEnv
  = TermEnv { term :: Terminal
            , cls :: LinesAffected -> TermOutput
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
  return $ TermEnv term' clearS

redraw :: Outputtable a => TermEnv -> a -> IO ()
redraw env x =
  let whatShouldThisBe = 1337
  in runTermOutput (term env) $ cls env whatShouldThisBe <> output x
