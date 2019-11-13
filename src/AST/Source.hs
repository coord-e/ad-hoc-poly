{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Source where

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
  | Tuple [Expr]
  | Let Name Expr Expr
  | Class [TypeName] TypeName [(Type, TypeName)] [(Name, Type)] Expr
  | Impl [TypeName] TypeName Type [(Type, TypeName)] [(Name, Expr)] Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr


-- Report instances
instance Report Expr where
  report = cata go
    where
      reportVars  = intercalate ", "
      reportPreds = intercalate ", " . map (\(t, n) -> report t ++ ": " ++ n)
      go (IntF i)       = show i
      go (CharF c)      = show c
      go (StrF s)       = show s
      go (RealF f)      = show f
      go (BoolF b)      = show b
      go (VarF x)       = x
      go (AppF e1 e2)   = paren (e1 ++ " " ++ e2)
      go (LamF x e)     = paren ("λ" ++ x ++ ". " ++ e)
      go (TupleF xs)    = paren (intercalate ", " xs)
      go (LetF x e1 e2) = "let " ++ x ++ " = " ++ e1 ++ " in\n" ++ e2
      go (ClassF as cls cs ms e) = "class<" ++ reportVars as ++ "> " ++ cls
                                   ++ " where " ++ reportPreds cs ++ " {\n" ++ ms' ++ "} in\n" ++ e
        where
          ms' = intercalate ",\n" $ map (\(n, t) -> n ++ " :: " ++ report t) ms
      go (ImplF as cls tgt cs ms e) = "impl<" ++ reportVars as ++ "> " ++ cls ++ " for " ++ report tgt
                                      ++ " where " ++ reportPreds cs ++ " {\n" ++ ms' ++ "} in\n" ++ e
        where
          ms' = intercalate ",\n" $ map (\(n, me) -> n ++ " = " ++ me) ms
