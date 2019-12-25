{-# LANGUAGE MultiParamTypeClasses #-}
module Reverse.Editor.Mode ( Mode(..), ModeUI(..), runMode ) where

import Data.Functor.Contravariant (Contravariant(..))

data Mode z s o a =
  Mode { recognize :: z Char -> z a
       , react :: a -> s -> Either o s
       }

data ModeUI z s
  = ModeUI { redraw :: s -> z ()
           , userInputChar :: z Char
           }

instance Contravariant (ModeUI z) where
  contramap f mui = mui { redraw = redraw mui . f }

runMode :: Monad z => Mode z s o a -> ModeUI z s -> s -> z o
runMode mode ui state = do
  redraw ui state
  action <- recognize mode $ userInputChar ui
  either return (runMode mode ui) $ react mode action state
