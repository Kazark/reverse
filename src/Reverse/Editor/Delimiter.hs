module Reverse.Editor.Delimiter
  ( Delim, delimToChar, charToDelim
  ) where

data Delim = Space | Underscore | Dash

delimToChar :: Delim -> Char
delimToChar = \case
  Space -> ' '
  Underscore -> '_'
  Dash -> '-'

charToDelim :: Char -> Maybe Delim
charToDelim = \case
  ' ' -> Just Space
  '_' -> Just Underscore
  '-' -> Just Dash
  _ -> Nothing
