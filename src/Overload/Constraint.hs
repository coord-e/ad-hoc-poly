{-# LANGUAGE FlexibleContexts #-}
module Overload.Constraint where

import           Overload.Env
import           Overload.Subst
import           Overload.Type
import           Overload.Var

import           Control.Eff
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Control.Monad.Extra       (allM, maybeM)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set


-- isInstance a b returns True if b is more general than a.
isInstance :: (Member Fresh r, Member (Reader Env) r) => TypeScheme -> TypeScheme -> Eff r Bool
isInstance (Forall as1 p1) t2@(Forall as2 p2) = do
  ts <- mapM (fmap TVar . const freshv) as2
  let s1 = Subst $ Map.fromList $ zip as1 ts
  let s2 = Subst $ Map.fromList $ zip as2 ts
  (&& Set.disjoint (ftv t2) (Set.fromList as1)) <$> isInstancePred (apply s1 p1) (apply s2 p2)

isInstancePred :: (Member Fresh r, Member (Reader Env) r) => PredType -> PredType -> Eff r Bool
isInstancePred (PredType cs1 t1) (PredType cs2 t2) = (t1 == t2 &&) <$> allM go cs2
  where
    go c@(Constraint x t) = do
      b <- bound x t
      i <- inst x t
      return (c `elem` cs1 || b || i)
    bound x t = maybe False (views _1 (== poly t)) <$> reader (views (context . bindings) (Map.lookup x))
    inst x t = maybeM (return False) (views _1 (isInstance (poly t))) $ reader (views (context . instantiations) (Map.lookup x))
    poly t = Forall [] (PredType [] t)
