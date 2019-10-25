{-# LANGUAGE FlexibleContexts #-}
module Overload.TypeEval where

import           AST.Source
import           Overload.Env

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.Map                  as Map


eval :: Member (Reader Env) r => Type -> Eff r Type
eval (TName x)               = reader $ flip (Map.!) x . view typeEnv
eval (TFun t1 t2)            = TFun <$> eval t1 <*> eval t2
eval (TTuple ts)             = TTuple <$> mapM eval ts
eval (TConstraint x t)       = TConstraint x <$> eval t
eval (TApp t1 t2) = do
  t1' <- eval t1
  t2' <- eval t2
  let (TLam x body) = t1'
  eval $ cata (subst x t2') body
eval t = return t

subst :: TypeName -> Type -> TypeF Type -> Type
subst n r (TNameF x) | x == n = r
subst _ _ t          = embed t
