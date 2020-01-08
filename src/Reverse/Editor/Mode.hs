{-# LANGUAGE MultiParamTypeClasses #-}
module Reverse.Editor.Mode ( Mode(..), ModeUI(..) ) where

import Data.Functor.Contravariant (Contravariant(..))

data Mode s o a =
  Mode { modeName :: String
       , actionMap :: [(Char, a)]
       , react :: a -> s -> Either o s
       , actionHelp :: a -> String
       }

data ModeUI z s
  = ModeUI { redraw :: s -> z ()
           , userInputChar :: z Char
           }

instance Contravariant (ModeUI z) where
  contramap f mui = mui { redraw = redraw mui . f }
