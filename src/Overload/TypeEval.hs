{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Overload.TypeEval where

import           AST.Name
import qualified AST.Type                  as S
import           Overload.Env
import           Overload.Type
import           Overload.Var

import           Control.Eff
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.Writer.Strict
import           Control.Lens
import qualified Data.Map                  as Map


type TyVarEnv = Map.Map TVarName TyVar

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
eval (S.TFun t1 t2)            = fmap SType . TFun <$> evalTy t1 <*> evalTy t2
  where
    evalTy = fmap expectTy . eval
eval (S.TPredicate c t)       = (tell =<< expectConstraint <$> eval c) >> eval t
eval (S.TApp t1 t2)           = do
  (x, body, env) <- expectClos <$> eval t1
  t2' <- eval t2
  local (set typeEnv_ $ Map.insert x (PredSem [] t2') env) $ eval body
eval (S.TLam x t) = SClosure x t <$> reader (view typeEnv_)
eval (S.TConstraint x s)       = SConstraint . Constraint x . expectTy <$> eval s


expectTy :: Sem -> Type
expectTy (SType t) = t
expectTy _         = error "a type is expected; invalid kind"

expectClos :: Sem -> (TypeName, S.Type, TypeEnv)
expectClos (SClosure x t env) = (x, t, env)
expectClos _                  = error "an abstraction is expected; invalid kind"

expectConstraint :: Sem -> Constraint
expectConstraint (SConstraint c) = c
expectConstraint _               = error "a constraint is expected; invalid kind"


runEvalWithVars :: (Member Fresh r, Member (Reader Env) r) => TyVarEnv -> S.Type -> Eff r PredSem
runEvalWithVars e t = do
  tyenv <- reader $ view typeEnv
  let env = TypeEvalEnv tyenv e
  (t', cs) <- runListWriter . runReader env $ eval t
  return $ PredSem cs t'

runEval :: (Member Fresh r, Member (Reader Env) r) => S.Type -> Eff r PredSem
runEval = runEvalWithVars Map.empty

runEvalToType :: (Member Fresh r, Member (Reader Env) r) => S.Type -> Eff r PredType
runEvalToType = fmap extract . runEval
  where
    extract (PredSem cs (SType t)) = PredType cs t
    extract _                      = error "something went wrong in kinding"

runSchemeEval :: (Member Fresh r, Member (Reader Env) r) => S.TypeScheme -> Eff r SemScheme
runSchemeEval (S.Forall as t) = do
  as' <- mapM (const freshv) as
  SForall as' <$> runEvalWithVars (Map.fromList $ zip as as') t

runSchemeEvalToType :: (Member Fresh r, Member (Reader Env) r) => S.TypeScheme -> Eff r TypeScheme
runSchemeEvalToType = fmap extract . runSchemeEval
  where
    extract (SForall as (PredSem cs (SType t))) = Forall as (PredType cs t)
    extract _               = error "something went wrong in kinding"
