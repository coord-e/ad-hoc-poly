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


data Kind
  = Star
  | Constraint
  | Arrow Kind Kind
  deriving Show

makeBaseFunctor ''Kind


data Type
  = TInt
  | TChar
  | TStr
  | TVar TVarName
  | TName TypeName
  | TFun Type Type
  | TApp Type Type
  | TLam TypeName Type
  | TTuple [Type]
  | TConstraint Name Type
  deriving Show

makeBaseFunctor ''Type


data TypeScheme
  = Forall { _vars        :: [TVarName],
             _constraints :: [Type],
             _type_       :: Type }
  deriving Show

makeLenses ''TypeScheme


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
  | Satisfy TypeScheme Expr Expr
  deriving Show

makeBaseFunctor ''Expr
