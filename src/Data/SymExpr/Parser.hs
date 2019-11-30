module Data.SymExpr.Parser (parseSymExpr) where

import Data.SymExpr.Types
import           Data.Void (Void)
import           Text.Megaparsec ( Parsec, between, try, (<|>), manyTill )
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (liftA2)
import Control.Monad (void)

type Parser = Parsec Void String

-- | sc - Space consumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

dot :: Parser ()
dot = void $ symbol "."

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

unit :: Parser (SymExpr s)
unit = return Unit

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* sc

atom :: Parser s -> Parser (SymExpr s)
atom parseSymbol =
  try (Text <$> stringLiteral)
  <|> (Symb <$> parseSymbol)

dotted :: Parser s -> Parser (SymExpr s)
dotted parseSymbol =
  liftA2 Cons (parseSymExpr parseSymbol <* dot) (atom parseSymbol)

proper :: Parser s -> Parser (SymExpr s)
proper parseSymbol =
  liftA2 Cons (parseSymExpr parseSymbol) (bare parseSymbol)

bare :: Parser s -> Parser (SymExpr s)
bare parseSymbol =
  try (dotted parseSymbol)
  <|> try (proper parseSymbol)
  <|> unit

cons :: Parser s -> Parser (SymExpr s)
cons = parens . bare

parseSymExpr :: Parser s -> Parser (SymExpr s)
parseSymExpr parseSymbol' =
  let parseSymbol = lexeme parseSymbol'
  in sc *> (try (atom parseSymbol) <|> cons parseSymbol)
