{-# LANGUAGE RankNTypes #-}
module Reverse.ModeHelp (ModeHelp(..)) where

data ModeHelp
  = ModeHelp { modeName :: String
             , describe :: forall z. Monad z => z Char -> z String
             }
