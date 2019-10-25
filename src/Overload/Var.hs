{-# LANGUAGE FlexibleContexts #-}
module Overload.Var where

import           Overload.Env
import           Overload.Subst
import           Overload.Type

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Lens
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set


fresh :: Member (State Infer) r => Eff r TyVar
fresh = do
  modify $ over unique (+1)
  TV . view unique <$> get

instantiate :: Member (State Infer) r => TypeScheme -> Eff r PredType
instantiate (Forall as (PredType cs t)) = do
  as' <- mapM (fmap TVar . const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ PredType cs $ apply s t

generalize :: Member (Reader Env) r => PredType -> Eff r TypeScheme
generalize p = do
  envFtv <- reader (views context ftv)
  let as = ftv p `Set.difference` envFtv
  return $ Forall (Set.toList as) p

