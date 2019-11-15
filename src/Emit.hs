module Emit where

import           AST.Literal
import           AST.Target
import           Reporting.Report      (paren)

import           Control.Exception     (assert)
import           Data.Bool             (bool)
import           Data.Functor.Foldable
import           Data.List


emit :: Expr -> String
emit = cata go
  where
    bars = flip replicate "_"
    go (LitF (Int i))   = show i
    go (LitF (Char c))  = show c
    go (LitF (Str s))   = show s
    go (LitF (Real f))  = show f
    go (LitF (Bool b))  = bool "false" "true" b
    go (VarF n)         = n
    go (AppF e1 e2)     = e1 ++ " " ++ paren e2
    go (LamF n e)       = paren ("fun " ++ n ++ " -> " ++ e)
    go (NthF n i e)     = assert (n > 1) $
                          "(fun (" ++ intercalate ", " (bars i ++ ["x"] ++ bars (n - i - 1)) ++ ") -> x) " ++ paren e
    go (TupleF es)      = paren $ intercalate ", " es
    go (LetF n e1 e2)   = "let " ++ n ++ " = " ++ e1 ++ " in\n" ++ e2
    go (PlaceholderF _) = error "placeholder is left in emit"
