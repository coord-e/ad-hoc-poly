module AST.Target where

type Name = String

data Expr
  = Int Int
  | Char Char
  | Str String
  | Var Name
  | App Expr Expr
  | Lam Name Expr
  | Tuple [Expr]
  | Let Name Expr Expr
