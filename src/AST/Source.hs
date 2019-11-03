{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Source where

import           Reporting.Report

import           Control.Lens.TH
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                (intercalate)


type TVarName = String
type TypeName = String
type Name = String


data Kind
  = Star
  | Constraint
  | Arrow Kind Kind
  deriving Show

makeBaseFunctor ''Kind


data Type
  = TVar TVarName
  | TName TypeName
  | TFun Type Type
  | TApp Type Type
  | TLam TypeName Type
  | TTuple [Type]
  | TPredicate Type Type
  | TConstraint Name TypeScheme
  deriving (Show, Eq)


data TypeScheme
  = Forall { _vars  :: [TVarName],
             _type_ :: Type }
  deriving (Show, Eq)

makeLenses ''TypeScheme
makeBaseFunctor ''Type


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
  | Type TypeName Type Expr
  | Over TypeScheme Expr
  | Satisfy TypeScheme Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr


-- Report instances
instance Report Kind where
  report = cata go
    where
      go StarF          = "*"
      go ConstraintF    = "C"
      go (ArrowF k1 k2) = paren k1 ++ " -> " ++ k2

instance Report Type where
  report = cata go
    where
      go (TVarF n)           = '\'' : n
      go (TNameF x)          = x
      go (TFunF t1 t2)       = paren t1 ++ " -> " ++ t2
      go (TAppF t1 t2)       = paren (t1 ++ " " ++ t2)
      go (TLamF x t)         = paren ("Λ" ++ x ++ ". " ++ t)
      go (TTupleF ts)        = paren (intercalate ", " ts)
      go (TPredicateF t1 t2) = t1 ++ " => " ++ t2
      go (TConstraintF x s)  = "constraint " ++ x ++ " " ++ paren (report s)

instance Report TypeScheme where
  report (Forall as t) = "∀" ++ unwords (map ('\'':) as) ++ ". " ++ report t

instance Report Expr where
  report = cata go
    where
      go (IntF i)       = show i
      go (CharF c)      = show c
      go (StrF s)       = show s
      go (RealF f)     = show f
      go (BoolF b)      = show b
      go (VarF x)       = x
      go (AppF e1 e2)   = paren (e1 ++ " " ++ e2)
      go (LamF x e)     = paren ("λ" ++ x ++ ". " ++ e)
      go (TupleF xs)    = paren (intercalate ", " xs)
      go (LetF x e1 e2) = "let " ++ x ++ " = " ++ e1 ++ " in\n" ++ e2
      go (TypeF x t e)   = "type " ++ x ++ " = " ++ report t ++ " in\n" ++ e
      go (OverF s e)     = "over " ++ paren (report s) ++ " in\n" ++ e
      go (SatisfyF s e1 e2) = "satisfy " ++ paren (report s) ++ " = " ++ e1 ++ " in\n" ++ e2
