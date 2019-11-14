{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Overload.LocalInfer where

import qualified AST.Intermediate          as S
import qualified AST.Literal               as S
import           AST.Name
import qualified AST.Target                as T
import qualified AST.Type                  as S
import           Config                    (LiteralTypes (..))
import           Overload.Env
import {-# SOURCE #-} Overload.GlobalInfer
import           Overload.Instance
import           Overload.Internal
import qualified Overload.Kind             as K
import           Overload.KindInfer        (kind, kindTo)
import           Overload.Type
import           Overload.TypeEval         (runEval, runEvalToType,
                                            runSchemeEval)
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
import           Control.Exception         (assert)
import           Control.Lens
import           Control.Monad             (replicateM)
import           Control.Monad.Extra       (fromMaybeM, maybeM, unlessM)
import           Data.Foldable             (foldlM)
import qualified Data.Map                  as Map


localInfer :: S.Expr -> Eff '[Writer Candidate, Fresh, Reader Env, State Constraints, Exc Error] (Type, T.Expr)
localInfer (S.Lit l@(S.Int _))    = (, T.Lit l) <$> literalType integer
localInfer (S.Lit l@(S.Char _))   = (, T.Lit l) <$> literalType char
localInfer (S.Lit l@(S.Str _))    = (, T.Lit l) <$> literalType string
localInfer (S.Lit l@(S.Real _))   = (, T.Lit l) <$> literalType real
localInfer (S.Lit l@(S.Bool _))   = (, T.Lit l) <$> literalType boolean
localInfer (S.Nth n i e)  = do
  ts <- replicateM n (TVar <$> freshv)
  (t, e') <- localInfer e
  unify (TTuple ts) t
  return (ts !! i, T.Nth n i e')
localInfer (S.Tuple xs) = bimap TTuple T.Tuple . unzip <$> mapM localInfer xs
localInfer (S.Lam x e)  = do
  tv <- TVar <$> freshv
  (ret, e') <- withBindingType x tv $ localInfer e
  return (TFun tv ret, T.Lam x e')
localInfer (S.App e1 e2) = do
  tv <- TVar <$> freshv
  (t1, e1') <- localInfer e1
  (t2, e2') <- localInfer e2
  unify t1 (TFun t2 tv)
  return (tv, T.App e1' e2')
localInfer (S.Var x) = maybeM (maybeM (throwError $ TypeError $ UnboundVariable x) inferVarOver overload) inferVarBound bound
  where
    bound = reader (views (context . bindings) (Map.lookup x))
    overload = reader (views (context . overloads) (Map.lookup x))
    inferVarBound s = resolvePredicates (T.Var x) =<< instantiate s
    inferVarOver s = do
      PredType _ t <- instantiate s
      i <- fresh
      c <- reader (view context)
      tell $ Candidate i x t c
      return (t, T.Placeholder i)
localInfer (S.Type x t e) = do
  k <- kind t
  s <- runEval t
  local (over typeEnv $ Map.insert x s) $ local (over kindEnv $ Map.insert x k) $ localInfer e
localInfer (S.Over s e) = do
  (x, s') <- extractConstraint s
  withOverload x s' $ localInfer e
localInfer (S.Satisfy sc e1 e2) = do
  (x, sc') <- extractConstraint sc
  ov <- fromMaybeM (throwError . TypeError $ NotOverloadedInstance x) $ reader (views (context . overloads) (Map.lookup x))
  unlessM (sc' `isInstance` ov) (throwError . TypeError $ InvalidInstance x ov sc')

  (t, e1i, wl) <- raise $ runLocalInfer e1
  unifySc t sc'
  (p1, e1') <- raise $ processWaitList t e1i wl
  s1 <- generalize p1

  unlessM (sc' `isInstance` s1) (throwError . TypeError $ UnableToInstantiate x s1 sc')
  n <- freshn x
  (t2, e2') <- withInstance x (sc', n) $ withBinding n s1 $ localInfer e2
  return (t2, T.Let n e1' e2')
  where
    unifySc t1 sc' = do
      PredType _ tc <- instantiate sc'
      unify t1 tc
localInfer (S.Let x e1 e2) = do
  (p1, e1') <- raise $ globalInfer e1
  s1 <- generalize p1
  (t2, e2') <- withBinding x s1 $ localInfer e2
  return (t2, T.Let x e1' e2')

runLocalInfer :: S.Expr -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] (Type, T.Expr, [Candidate])
runLocalInfer e = do
  ((t, e'), wl) <- runListWriter $ localInfer e
  return (t, e', wl)


resolvePredicates :: T.Expr -> PredType -> Eff '[Writer Candidate, Fresh, Reader Env, State Constraints, Exc Error] (Type, T.Expr)
resolvePredicates e (PredType [] t) = return (t, e)
resolvePredicates e (PredType cs t) = do
  e' <- foldlM go e cs
  return (t, e')
  where
    go ae (Constraint x tc) = do
      (tx, ex) <- localInfer $ S.Var x
      unify tx tc
      return (T.App ae ex)

extractConstraint :: (Member (Exc Error) r, Member Fresh r, Member (Reader Env) r) => S.TypeScheme -> Eff r (Name, TypeScheme)
extractConstraint s@(S.Forall _ t) = do
  kindTo t K.Constraint
  SForall as (PredSem cs t') <- runSchemeEval s
  let Constraint x t'' = extract t'
  return (x, Forall as (PredType cs t''))
  where
    extract (SConstraint c) = c
    extract _               = error "something went wrong in kinding"


withInstance :: Member (Reader Env) r => Name -> (TypeScheme, Name) -> Eff r a -> Eff r a
withInstance x i = local (over (context . instantiations) (adjustWithDefault (i:) [i] x))

withBinding :: Member (Reader Env) r => Name -> TypeScheme -> Eff r a -> Eff r a
withBinding x s = local (over (context . bindings) (Map.insert x s))

withBindingType :: Member (Reader Env) r => Name -> Type -> Eff r a -> Eff r a
withBindingType x = withBinding x . scheme

withOverload :: Member (Reader Env) r => Name -> TypeScheme -> Eff r a -> Eff r a
withOverload x t = local (over (context . overloads) (Map.insert x t))

literalType :: (Member (Exc Error) r, Member (Reader Env) r, Member Fresh r) => (LiteralTypes -> S.Type) -> Eff r Type
literalType f = do
  -- TODO: it is inefficient to evalutate S.TypeScheme at every literals' occurence
  t <- f <$> reader (view literalTypes)
  kindTo t K.Star
  PredType cs t' <- runEvalToType t
  assert (null cs) $ return t'

freshn :: Member Fresh r => String -> Eff r Name
freshn base = do
  v <- fresh
  return (base ++ "_" ++ show v)
