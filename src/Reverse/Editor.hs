module Reverse.Editor ( Action, Mode, ingest ) where

import Reverse.Editor.NormalMode (ingest)
import qualified Reverse.Editor.NormalMode as NormalMode

type Action = NormalMode.Action
  -- | Divide

type Mode = NormalMode.Normal
