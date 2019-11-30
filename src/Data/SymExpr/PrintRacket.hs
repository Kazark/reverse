module Data.SymExpr.PrintRacket (printRExpr) where

import Prelude hiding (print)
import Data.SymExpr.Racket
import Data.List.Utils (replace)

printBlockComment :: String -> String
printBlockComment comment = "#|" ++ comment ++ "|#\n"

printCdr :: Show s => Bool -> RExpr s -> RExpr s -> String
printCdr parens car = \case
  Nil -> ""
  BlockComment comment cdr -> printBlockComment comment ++ printCdr parens car cdr
  cdr@(ConsC _ _) -> ' ' : printRExpr' False cdr
  cdr -> " . " ++ printRExpr' True cdr

printRExpr' :: Show s => Bool -> RExpr s -> String
printRExpr' parens =
  \case
    BlockComment comment on -> printBlockComment comment ++ printRExpr' parens on
    ConsC car cdr ->
      let
        d1 = printRExpr' True car
        d2 = printCdr parens car cdr
        d = d1 ++ d2
      in if parens then '(' : d ++ ")" else d
    Integer i -> show i
    Nil -> "()"
    Quasiquote e -> '`' : printRExpr' True e
    Quote e -> '\'' : printRExpr' True e
    Str s -> '"' : replace "\"" "\\\"" s ++ "\""
    KnownSymbol s -> show s
    Symbol s -> show s
    Unquote e -> ',' : printRExpr' True e

printRExpr :: Show s => RExpr s -> String
printRExpr = printRExpr' True
