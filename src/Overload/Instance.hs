{-# LANGUAGE FlexibleContexts #-}
module Overload.Instance where

import qualified AST.Source                as S
import           Overload.Env
import           Overload.Subst
import           Overload.Type
import           Overload.Var

import           Control.Eff
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Control.Monad             (join)
import           Control.Monad.Extra       (allM, findM)
import qualified Data.Map                  as Map
import           Data.Maybe                (isJust)
import qualified Data.Set                  as Set


-- isInstance a b returns True if b is more general than a.
isInstance :: (Member Fresh r, Member (Reader Env) r) => TypeScheme -> TypeScheme -> Eff r Bool
isInstance (Forall as1 p1) t2@(Forall as2 p2) = do
  ts <- mapM (fmap TVar . const freshv) as2
  let s1 = Subst $ Map.fromList $ zip as1 ts
  let s2 = Subst $ Map.fromList $ zip as2 ts
  (&& Set.disjoint (ftv t2) (Set.fromList as1)) <$> isInstancePred (apply s1 p1) (apply s2 p2)

isInstancePred :: (Member Fresh r, Member (Reader Env) r) => PredType -> PredType -> Eff r Bool
isInstancePred (PredType cs1 t1) (PredType cs2 t2) = (t1 == t2 &&) <$> allM check cs2
  where
    check c = (|| c `elem` cs1) <$> canBeEliminated c

findInstantiation :: (Member Fresh r, Member (Reader Env) r) => S.Name -> TypeScheme -> Eff r (Maybe (TypeScheme, S.Expr))
findInstantiation x s = fmap join . mapM (findM . views _1 $ isInstance s) =<< reader (views (context . instantiations) $ Map.lookup x)

canBeEliminated :: (Member Fresh r, Member (Reader Env) r) => Constraint -> Eff r Bool
canBeEliminated (Constraint x s) = (||) <$> bound <*> inst
  where
    bound = maybe False (views _1 (== s)) <$> reader (views (context . bindings) $ Map.lookup x)
    inst = isJust <$> findInstantiation x s
