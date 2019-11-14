{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Intermediate where

import           AST.Name
import           AST.Type
import           Reporting.Report

import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                (intercalate)


data Expr
  = Int Int
  | Char Char
  | Str String
  | Real Double
  | Bool Bool
  | Var Name
  | App Expr Expr
  | Lam Name Expr
  | Nth Int Int Expr
  | Tuple [Expr]
  | Let Name Expr Expr
  | Type TypeName Type Expr
  | Over TypeScheme Expr
  | Satisfy TypeScheme Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr


-- Report instances
instance Report Expr where
  report = cata go
    where
      go (IntF i)       = show i
      go (CharF c)      = show c
      go (StrF s)       = show s
      go (RealF f)      = show f
      go (BoolF b)      = show b
      go (VarF x)       = x
      go (AppF e1 e2)   = paren (e1 ++ " " ++ e2)
      go (LamF x e)     = paren ("λ" ++ x ++ ". " ++ e)
      go (NthF _ i e)   = paren e ++ "#" ++ show i
      go (TupleF xs)    = paren (intercalate ", " xs)
      go (LetF x e1 e2) = "let " ++ x ++ " = " ++ e1 ++ " in\n" ++ e2
      go (TypeF x t e)  = "type " ++ x ++ " = " ++ report t ++ " in\n" ++ e
      go (OverF s e)    = "over " ++ paren (report s) ++ " in\n" ++ e
      go (SatisfyF s e1 e2) = "satisfy " ++ paren (report s) ++ " = " ++ e1 ++ " in\n" ++ e2
