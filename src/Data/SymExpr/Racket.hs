-- | A subset of Racket that is a superset of the s-expressions we support
-- | deserializing, useful as a metasyntax.
module Data.SymExpr.Racket
  ( KnownSymbol(..), RExpr(..), toRExpr, rExprList, apN, apK, isInfix
  ) where

import Data.List (foldr)
import Data.SymExpr.Types

-- | Racket symbol that we intend to use as part of the metasyntax.
data KnownSymbol
  = ConsQ -- cons?
  | Display -- display
  | EmptyQ -- empty?
  | Length -- length
  | LessThan -- <
  | Let -- let
  | ListQ -- list?
  | Memq -- memq
  | Or -- or
  | Record -- This is not a Racket builtin, but is for use with let
  | StringQ -- string?
  | SymbolQ -- symbol?
  | Void -- void

isInfix :: KnownSymbol -> Bool
isInfix = \case LessThan -> True; _ -> False

instance Show KnownSymbol where
  show = \case
    ConsQ -> "cons?"
    Display -> "display"
    EmptyQ -> "empty?"
    Length -> "length"
    LessThan -> "<"
    Let -> "let"
    ListQ -> "list?"
    Memq -> "memq"
    Or -> "or"
    Record -> "record"
    StringQ -> "string?"
    SymbolQ -> "symbol?"
    Void -> "void"

-- | A Racket expression (much smaller than Racket's actual AST, no doubt)
-- | Technically, this could all be represented as an S-expression. I, however,
-- | believe that if you are honest about the AST of Racket, or even a
-- | Scheme---or perhaps, who knows, maybe even original Lisp---the way it
-- | appears as S-expressions is a particular encoding, but is less direct than
-- | the encoding as an algebraic data type (*ducks, runs, hides*). In any case,
-- | for our purposes, it will be more efficient and easier to work with to make
-- | the AST fully explicit, rather than obscurely treating it as hidden
-- | in/muddled into S-expressions (not to mention, more performant).
data RExpr s
  = BlockComment String (RExpr s)
  | ConsC (RExpr s) (RExpr s)
  | Integer Integer
  | Nil
  | Quasiquote (RExpr s)
  | Quote (RExpr s)
  | Str String
  | KnownSymbol KnownSymbol
  | Symbol s
  | Unquote (RExpr s)

-- | Lift a symbolic expression into the metasyntax
toRExpr :: SymExpr s -> RExpr s
toRExpr (Cons x y) = ConsC (toRExpr x) (toRExpr y)
toRExpr Unit = Nil
toRExpr (Symb s) = Symbol s
toRExpr (Text x) = Str x

rExprList :: [RExpr s] -> RExpr s
rExprList = foldr ConsC Nil

-- | Apply a known function to _n_ arguments in the metasyntax
apN :: KnownSymbol -> [RExpr s] -> RExpr s
apN f = ConsC (KnownSymbol f) . rExprList

-- | Apply a known function to one argument in the metasyntax
apK :: KnownSymbol -> RExpr s -> RExpr s
apK f x = apN f [x]
