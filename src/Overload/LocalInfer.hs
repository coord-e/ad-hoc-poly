{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Overload.LocalInfer where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import {-# SOURCE #-} Overload.GlobalInfer
import           Overload.Instance
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
import           Control.Eff.Extend        (raise)
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Eff.Writer.Strict
import           Control.Lens
import           Control.Monad.Extra       (maybeM, unlessM)
import           Data.Bifunctor
import qualified Data.Map                  as Map


localInfer :: S.Expr -> Eff '[Writer Candidate, Fresh, Reader Env, State Constraints, Exc Error] (PredType, T.Expr)
localInfer (S.Int i)    = return (predt TInt, T.Int i)
localInfer (S.Char c)   = return (predt TChar, T.Char c)
localInfer (S.Str s)    = return (predt TStr, T.Str s)
localInfer (S.Tuple xs) = bimap (overpred TTuple) T.Tuple . unzip <$> mapM localInfer xs
localInfer (S.Lam x e)  = do
  tv <- TVar <$> freshv
  (PredType cs ret, e') <- withBinding x (scheme $ predt tv) (S.Var x) $ localInfer e
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
    -- NOTE: guard to avoid infinity loop
    inferVarBound (s, S.Var x') | x == x' = (, T.Var x) <$> instantiate s
    inferVarBound (s, e) = do
      p <- instantiate s
      (p', e') <- localInfer e
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
  (x, s') <- extractConstraint s
  withOverload x s' $ localInfer e
localInfer (S.Satisfy sc e1 e2) = do
  (x, sc') <- extractConstraint sc
  (p1, e1', left) <- raise $ globalInfer e1
  s1 <- generalize p1
  unlessM (s1 `isInstance` sc') (throwError . TypeError $ UnableToInstantiate x s1 sc')
  -- TODO: check overlapping instances
  n <- freshn x
  let inst = (sc', applyLeft n left)
  (p2, e2') <- withInstance x inst $ localInfer e2
  return (p2, T.Let n e1' e2')
localInfer (S.Let x e1 e2) = do
  (p1, e1', left) <- raise $ globalInfer e1
  s1 <- generalize p1
  n <- freshn x
  (p2, e2') <- withBinding x s1 (applyLeft n left) . withBinding n s1 (S.Var n) $ localInfer e2
  return (p2, T.Let n e1' e2')

runLocalInfer :: S.Expr -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] (PredType, T.Expr, [Candidate])
runLocalInfer e = do
  ((p, e'), wl) <- runListWriter $ localInfer e
  return (p, e', wl)


extractConstraint :: (Member (Exc Error) r, Member Fresh r, Member (Reader Env) r) => S.TypeScheme -> Eff r (S.Name, TypeScheme)
extractConstraint s@(S.Forall _ t) = do
  kindTo t K.Constraint
  SForall as (PredSem cs t') <- runSchemeEval s
  let Constraint x (Forall as' (PredType cs' t'')) = extract t'
  return (x, Forall (as ++ as') (PredType (cs ++ cs') t''))
  where
    extract (SConstraint c) = c
    extract _               = error "something went wrong in kinding"

-- > applyLeft "n" ["a", "b", "c"]
-- App (App (App (Var "n") (Var "a")) (Var "b")) (Var "c")
applyLeft :: S.Name -> [S.Name] -> S.Expr
applyLeft n = foldl ((. S.Var) . S.App) (S.Var n)

withInstance :: Member (Reader Env) r => S.Name -> (TypeScheme, S.Expr) -> Eff r a -> Eff r a
withInstance x i = local (over (context . instantiations) (Map.adjust (i:) x))

withBinding :: Member (Reader Env) r => S.Name -> TypeScheme -> S.Expr -> Eff r a -> Eff r a
withBinding x t e = local (over (context . bindings) (Map.insert x (t, e)))

withOverload :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r a -> Eff r a
withOverload x t = local (over (context . overloads) (Map.insert x t))

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
