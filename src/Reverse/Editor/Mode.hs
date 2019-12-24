{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Reverse.Editor.Mode (Mode(..)) where

class Mode m a | a -> m where
  recognizeAction :: Monad z => z Char -> z (Maybe a)
  act :: a -> m -> m
