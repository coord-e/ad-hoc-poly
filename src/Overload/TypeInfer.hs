{-# LANGUAGE FlexibleContexts #-}
module Overload.TypeInfer where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import           Overload.Type
import           Overload.Var
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Eff.Writer.Strict
import           Control.Lens
import           Data.Bifunctor
import qualified Data.Map                  as Map


infer :: (Member Fresh r, Member (Reader Env) r, Member (State Constraints) r, Member (Writer WaitList) r, Member (Exc Error) r) => S.Expr -> Eff r (PredType, T.Expr)
infer (S.Int i)    = return (predt TInt, T.Int i)
infer (S.Char c)   = return (predt TChar, T.Char c)
infer (S.Str s)    = return (predt TStr, T.Str s)
infer (S.Tuple xs) = bimap (overpred TTuple) T.Tuple . unzip <$> mapM infer xs
infer (S.Lam x e)  = do
  tv <- TVar <$> freshv
  (PredType cs ret, e') <- bindingT x tv $ infer e
  return (PredType cs (TFun tv ret), T.Lam x e')
infer (S.App e1 e2) = do
  tv <- TVar <$> freshv
  (PredType cs1 t1, e1') <- infer e1
  (PredType cs2 t2, e2') <- infer e2
  unify t1 (TFun t2 tv)
  return (PredType (cs1 ++ cs2) tv, T.App e1' e2')

binding :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r a -> Eff r a
binding x t = local (over (context . bindings) (Map.insert x (t, T.Var x)))

bindingT :: Member (Reader Env) r => S.Name -> Type -> Eff r a -> Eff r a
bindingT x = binding x . scheme . predt

scheme :: PredType -> TypeScheme
scheme = Forall []

predt :: Type -> PredType
predt = PredType []

overpred :: ([Type] -> Type) -> [PredType] -> PredType
overpred f = uncurry PredType . second f . foldr go ([], [])
  where
    go (PredType cs t) (acs, ats) = (acs ++ cs, t : ats)

unify :: Member (State Constraints) r => Type -> Type -> Eff r ()
unify t1 t2 = modify ((t1, t2):)
