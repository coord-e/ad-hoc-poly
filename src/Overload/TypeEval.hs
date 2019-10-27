{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Overload.TypeEval where

import qualified AST.Source                as S
import           Overload.Env
import           Overload.Subst
import           Overload.Type
import           Overload.Var

import           Control.Eff
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.Writer.Strict
import           Control.Lens
import           Control.Monad
import           Data.Functor.Foldable
import           Data.List                 (partition)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set


type TyVarEnv = Map.Map S.TVarName TyVar

data TypeEvalEnv
  = TypeEvalEnv { _typeEnv_ :: TypeEnv
                , _typeVars :: TyVarEnv }

makeLenses ''TypeEvalEnv


eval :: (Member Fresh r, Member (Writer Constraint) r, Member (Reader TypeEvalEnv) r) => S.Type -> Eff r Sem
eval (S.TName x)               = do
  PredSem cs t <- reader (flip (Map.!) x . view typeEnv_)
  mapM_ tell cs
  return t
eval (S.TVar x)                = SType . TVar <$> reader (flip (Map.!) x . view typeVars)
eval (S.TTuple ts)             = SType . TTuple <$> mapM (fmap expectTy . eval) ts
eval (S.TFun t1 t2)            = SType <$> (TFun <$> evalTy t1 <*> evalTy t2)
  where
    evalTy = fmap expectTy . eval
eval (S.TPredicate c t)       = (tell =<< expectConstraint <$> eval c) >> eval t
eval (S.TApp t1 t2)           = do
  (x, body, env) <- expectClos <$> eval t1
  t2' <- eval t2
  local (set typeEnv_ $ Map.insert x (PredSem [] t2') env) $ eval body
eval (S.TLam x t) = SClosure x t <$> reader (view typeEnv_)
eval (S.TConstraint x s)       = SConstraint . Constraint x <$> evalScheme s
  where
    evalScheme (S.Forall as t) = do
      as' <- mapM (const freshv) as
      (t', cs) <- runListWriter $ expectTy <$> local (over typeVars $ adding as as') (eval t)
      let (outer, inner) = partition (isBoundBy $ Set.fromList as') cs
      mapM_ tell outer
      return $ Forall as' (PredType inner t')
    isBoundBy as c = Set.disjoint (ftv c) as
    go (a, a') = Map.insert a a'
    adding ks vs m = foldr go m (zip ks vs)


expectTy :: Sem -> Type
expectTy (SType t) = t
expectTy _         = error "a type is expected; invalid kind"

expectClos :: Sem -> (S.TypeName, S.Type, TypeEnv)
expectClos (SClosure x t env) = (x, t, env)
expectClos _                  = error "an abstraction is expected; invalid kind"

expectConstraint :: Sem -> Constraint
expectConstraint (SConstraint c) = c
expectConstraint _               = error "a constraint is expected; invalid kind"


runEvalWithVars :: (Member Fresh r, Member (Reader Env) r) => TyVarEnv -> S.Type -> Eff r PredSem
runEvalWithVars e t = do
  tyenv <- reader $ view typeEnv
  let env = TypeEvalEnv tyenv e
  (t, cs) <- runListWriter . runReader env $ eval t
  return $ PredSem cs t

runEval :: (Member Fresh r, Member (Reader Env) r) => S.Type -> Eff r PredSem
runEval = runEvalWithVars Map.empty

runSchemaEval :: (Member Fresh r, Member (Reader Env) r) => S.TypeScheme -> Eff r SemScheme
runSchemaEval (S.Forall as t) = do
  as' <- mapM (const freshv) as
  SForall as' <$> runEvalWithVars (Map.fromList $ zip as as') t
