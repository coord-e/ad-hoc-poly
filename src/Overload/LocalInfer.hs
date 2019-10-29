{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Overload.LocalInfer where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import qualified Overload.Kind             as K
import           Overload.KindInfer        (kind, kindTo)
import           Overload.Type
import           Overload.TypeEval         (runEval, runSchemeEval)
import           Overload.Unify
import           Overload.Var
import           Reporting.Error
import           Reporting.Error.Type

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Eff.Writer.Strict
import           Control.Lens
import           Control.Monad.Extra       (maybeM)
import           Data.Bifunctor
import qualified Data.Map                  as Map


localInfer :: S.Expr -> Eff '[Writer Candidate, Exc Error, Fresh, Reader Env, State Constraints] (PredType, T.Expr)
localInfer (S.Int i)    = return (predt TInt, T.Int i)
localInfer (S.Char c)   = return (predt TChar, T.Char c)
localInfer (S.Str s)    = return (predt TStr, T.Str s)
localInfer (S.Tuple xs) = bimap (overpred TTuple) T.Tuple . unzip <$> mapM localInfer xs
localInfer (S.Lam x e)  = do
  tv <- TVar <$> freshv
  (PredType cs ret, e') <- bindingT x tv $ localInfer e
  return (PredType cs (TFun tv ret), T.Lam x e')
localInfer (S.App e1 e2) = do
  tv <- TVar <$> freshv
  (PredType cs1 t1, e1') <- localInfer e1
  (PredType cs2 t2, e2') <- localInfer e2
  unify t1 (TFun t2 tv)
  return (PredType (cs1 ++ cs2) tv, T.App e1' e2')
localInfer (S.Var x) = maybeM (maybeM (throwError $ TypeError $ UnboundVariable x) inferVarOver overload) inferVarBound bound
  where
    bound = reader (views (context . bindings) (Map.lookup x))
    overload = reader (views (context . overloads) (Map.lookup x))
    inferVarBound (s, e) = do
      p <- instantiate s
      (p', e') <- localInfer e  -- TODO: infinity loop
      (, e') <$> unifyP p p'
    inferVarOver s = do
      p <- instantiate s
      i <- fresh
      tell $ Candidate i x p
      return (p, T.Placeholder i)
localInfer (S.Type x t e) = do
  k <- kind t
  s <- runEval t
  local (over typeEnv $ Map.insert x s) $ local (over kindEnv $ Map.insert x k) $ localInfer e
localInfer (S.Over s e) = do
  kindTo' s K.Constraint
  SForall as (PredSem cs t) <- runSchemeEval s
  let (Constraint x (Forall as' (PredType cs' t'))) = extract t
  let s' = Forall (as ++ as') (PredType (cs ++ cs') t')
  local (over (context . overloads) $ Map.insert x s') $ localInfer e
  where
    extract (SConstraint c) = c
    kindTo' (S.Forall _ t) = kindTo t

runLocalInfer :: S.Expr -> Eff '[Exc Error, Fresh, Reader Env, State Constraints] (PredType, T.Expr, [Candidate])
runLocalInfer e = do
  ((p, e'), wl) <- runListWriter $ localInfer e
  return (p, e', wl)


binding :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r a -> Eff r a
binding x t = local (over (context . bindings) (Map.insert x (t, S.Var x)))

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

addpred :: Constraint -> PredType -> PredType
addpred c (PredType cs t) = PredType (c:cs) t

freshn :: Member Fresh r => String -> Eff r T.Name
freshn base = do
  v <- fresh
  return (base ++ "_" ++ show v)
