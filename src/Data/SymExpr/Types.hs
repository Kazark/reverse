module Data.SymExpr.Types (SymExpr(..)) where

-- | Symbolic expressions with integers and strings parameterized over the
-- | symbol type
data SymExpr s
  = Unit
  | Symb s
  | Text String
  | Cons (SymExpr s) (SymExpr s)
