{-# LANGUAGE FlexibleContexts #-}
module Overload.Var where

import           Overload.Env
import           Overload.Subst
import           Overload.Type

import           Control.Eff
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Lens
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set


freshv :: Member Fresh r => Eff r TyVar
freshv = TV <$> fresh

instantiate :: Member Fresh r => TypeScheme -> Eff r PredType
instantiate (Forall as p) = do
  as' <- mapM (fmap TVar . const freshv) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s p

generalize :: Member (Reader Env) r => PredType -> Eff r TypeScheme
generalize p = do
  envFtv <- reader (views context ftv)
  let as = ftv p `Set.difference` envFtv
  return $ Forall (Set.toList as) p

