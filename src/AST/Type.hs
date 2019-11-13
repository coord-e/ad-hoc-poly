{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Type where

import           AST.Name
import           Reporting.Report

import           Control.Lens.TH
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                (intercalate)


data Type
  = TVar TVarName
  | TName TypeName
  | TFun Type Type
  | TApp Type Type
  | TLam TypeName Type
  | TTuple [Type]
  | TPredicate Type Type
  | TConstraint Name Type
  deriving (Show, Eq)


data TypeScheme
  = Forall { _vars  :: [TVarName],
             _type_ :: Type }
  deriving (Show, Eq)

makeLenses ''TypeScheme
makeBaseFunctor ''Type


-- Report instances
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
      go (TConstraintF x t)  = "constraint " ++ x ++ " :: " ++ paren t

instance Report TypeScheme where
  report (Forall as t) = "∀" ++ unwords (map ('\'':) as) ++ ". " ++ report t
