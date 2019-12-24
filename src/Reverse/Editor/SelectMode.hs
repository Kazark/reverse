{-# LANGUAGE MultiParamTypeClasses #-}
module Reverse.Editor.SelectMode
  ( Select(..), Action(..)
  ) where

import Reverse.Editor.Mode
import Reverse.Model

newtype Select = Select (Model [Cell])

data Action
  = Unsplit

instance Mode Select Action where
  recognizeAction _getChr = return Nothing
  act _ = id
