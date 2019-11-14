module Emit where

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
    go (IntF i)         = show i
    go (CharF c)        = show c
    go (StrF s)         = show s
    go (RealF f)        = show f
    go (BoolF b)        = bool "false" "true" b
    go (VarF n)         = n
    go (AppF e1 e2)     = e1 ++ " " ++ paren e2
    go (LamF n e)       = paren ("fun " ++ n ++ " -> " ++ e)
    go (NthF n i e)     = assert (n > 1) $
                          "(fun (" ++ intercalate ", " (bars i ++ ["x"] ++ bars (n - i - 1)) ++ ") -> x) " ++ paren e
    go (TupleF es)      = paren $ intercalate ", " es
    go (LetF n e1 e2)   = "let " ++ n ++ " = " ++ e1 ++ " in\n" ++ e2
    go (PlaceholderF _) = error "placeholder is left in emit"
