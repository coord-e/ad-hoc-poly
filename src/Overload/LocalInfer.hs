{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Overload.LocalInfer where

import qualified AST.Source                as S
import qualified AST.Target                as T
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
import           Control.Monad.Extra       (maybeM, unlessM, whenM)
import           Data.Foldable             (foldlM)
import qualified Data.Map                  as Map


localInfer :: S.Expr -> Eff '[Writer Candidate, Fresh, Reader Env, State Constraints, Exc Error] (Type, T.Expr)
localInfer (S.Int i)    = (, T.Int i) <$> literalType integer
localInfer (S.Char c)   = (, T.Char c) <$> literalType char
localInfer (S.Str s)    = (, T.Str s) <$> literalType string
localInfer (S.Real f)   = (, T.Real f) <$> literalType real
localInfer (S.Bool b)   = (, T.Bool b) <$> literalType boolean
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
      p <- instantiate s
      i <- fresh
      (t, e) <- resolvePredicates (T.Placeholder i) p
      c <- reader (view context)
      tell $ Candidate i x t c
      return (t, e)
localInfer (S.Type x t e) = do
  k <- kind t
  s <- runEval t
  local (over typeEnv $ Map.insert x s) $ local (over kindEnv $ Map.insert x k) $ localInfer e
localInfer (S.Over s e) = do
  (x, s') <- extractConstraint s
  withOverload x s' $ localInfer e
localInfer (S.Satisfy sc e1 e2) = do
  (x, sc') <- extractConstraint sc
  whenM (isOverlapping x sc') (throwError . TypeError $ OverlappingInstance x sc')
  (s1, e1') <- raise $ globalInfer e1
  unlessM ((&&) <$> sc' `isInstance` s1 <*> s1 `isInstance` sc') (throwError . TypeError $ UnableToInstantiate x s1 sc')
  n <- freshn x
  (t2, e2') <- withInstance x (sc', n) $ withBinding n s1 $ localInfer e2
  return (t2, T.Let n e1' e2')
localInfer (S.Let x e1 e2) = do
  (s1, e1') <- raise $ globalInfer e1
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

extractConstraint :: (Member (Exc Error) r, Member Fresh r, Member (Reader Env) r) => S.TypeScheme -> Eff r (S.Name, TypeScheme)
extractConstraint s@(S.Forall _ t) = do
  kindTo t K.Constraint
  SForall as (PredSem cs t') <- runSchemeEval s
  let Constraint x t'' = extract t'
  return (x, Forall as (PredType cs t''))
  where
    extract (SConstraint c) = c
    extract _               = error "something went wrong in kinding"


withInstance :: Member (Reader Env) r => S.Name -> (TypeScheme, T.Name) -> Eff r a -> Eff r a
withInstance x i = local (over (context . instantiations) (adjustWithDefault (i:) [i] x))

withBinding :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r a -> Eff r a
withBinding x s = local (over (context . bindings) (Map.insert x s))

withBindingType :: Member (Reader Env) r => S.Name -> Type -> Eff r a -> Eff r a
withBindingType x = withBinding x . scheme

withOverload :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r a -> Eff r a
withOverload x t = local (over (context . overloads) (Map.insert x t))

literalType :: (Member (Exc Error) r, Member (Reader Env) r, Member Fresh r) => (LiteralTypes -> S.Type) -> Eff r Type
literalType f = do
  -- TODO: it is inefficient to evalutate S.TypeScheme at every literals' occurence
  t <- f <$> reader (view literalTypes)
  kindTo t K.Star
  PredType cs t' <- runEvalToType t
  assert (null cs) $ return t'

freshn :: Member Fresh r => String -> Eff r T.Name
freshn base = do
  v <- fresh
  return (base ++ "_" ++ show v)
