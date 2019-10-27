{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Source where

import           Control.Lens.TH
import           Data.Functor.Foldable.TH


type TVarName = String
type TypeName = String
type Name = String


data Type
  = TVar TVarName
  | TName TypeName
  | TFun Type Type
  | TApp Type Type
  | TLam TypeName Type
  | TTuple [Type]
  | TPredicate Type Type
  | TConstraint Name TypeScheme
  deriving Show


data TypeScheme
  = Forall { _vars  :: [TVarName],
             _type_ :: Type }
  deriving Show

makeLenses ''TypeScheme
makeBaseFunctor ''Type


data Expr
  = Int Int
  | Char Char
  | Str String
  | Var Name
  | App Expr Expr
  | Lam Name Expr
  | Tuple [Expr]
  | Let Name Expr Expr
  | Type TypeName Type Expr
  | Over TypeScheme Expr
  | Satisfy TypeScheme Expr Expr
  deriving Show

makeBaseFunctor ''Expr
