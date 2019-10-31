module Emit where

import           AST.Target
import           Reporting.Report      (paren)

import           Data.Bool             (bool)
import           Data.Functor.Foldable
import           Data.List


emit :: Expr -> String
emit = cata emit'

emit' :: ExprF String -> String
emit' (IntF i)       = show i
emit' (CharF c)      = show c
emit' (StrF s)       = show s
emit' (RealF f)      = show f
emit' (BoolF b)      = bool "false" "true" b
emit' (VarF n)       = n
emit' (AppF e1 e2)   = e1 ++ " " ++ paren e2
emit' (LamF n e)     = paren ("fun " ++ n ++ " -> " ++ e)
emit' (TupleF es)    = paren $ intercalate ", " es
emit' (LetF n e1 e2) = "let " ++ n ++ " = " ++ e1 ++ " in\n" ++ e2
