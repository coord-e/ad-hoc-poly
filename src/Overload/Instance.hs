{-# LANGUAGE FlexibleContexts #-}
module Overload.Instance where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import           Overload.Subst
import           Overload.Type
import           Overload.Unify

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Control.Monad             (join)
import           Control.Monad.Extra       (allM, anyM, findM)
import           Data.Either.Extra         (eitherToMaybe)
import qualified Data.Map                  as Map
import           Data.Maybe                (isJust)
import qualified Data.Set                  as Set


-- isInstance a b returns True if b is more general than a.
isInstance :: Member (Reader Env) r => TypeScheme -> TypeScheme -> Eff r Bool
isInstance (Forall as1 p1) t2@(Forall _as2 p2) = (&& Set.disjoint (ftv t2) (Set.fromList as1)) <$> isInstancePred p1 p2

isInstancePred :: Member (Reader Env) r => PredType -> PredType -> Eff r Bool
isInstancePred (PredType cs1 t1) (PredType cs2 t2) = maybe (return False) checkAll $ isInstanceType t1 t2
  where
    check cs c = (|| c `elem` cs) <$> canBeEliminated c
    checkAll s = allM (check $ apply s cs1) (apply s cs2)

isInstanceType :: Type -> Type -> Maybe Subst
isInstanceType t1 t2 = do
  s@(Subst m) <- eitherToMaybe $ runUnifyAndSolve t2 t1
  toMaybe (disjointKeys m $ ftv t1) s

findInstantiation :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r (Maybe (TypeScheme, T.Name))
findInstantiation x s = fmap join . mapM (findM . views _1 $ isInstance s) =<< reader (views (context . instantiations) $ Map.lookup x)

findInstantiationType :: Member (Reader Env) r => S.Name -> Type -> Eff r (Maybe (TypeScheme, T.Name))
findInstantiationType x = findInstantiation x . Forall [] . PredType []

canBeEliminated :: Member (Reader Env) r => Constraint -> Eff r Bool
canBeEliminated (Constraint x t) = (||) <$> bound <*> inst
  where
    s = Forall [] $ PredType [] t
    bound = (== Just s) <$> reader (views (context . bindings) $ Map.lookup x)
    inst = isJust <$> findInstantiation x s

isOverlapping :: Member (Reader Env) r => S.Name -> TypeScheme -> Eff r Bool
isOverlapping x s = maybe (return False) (anyM check) =<< reader (views (context . instantiations) $ Map.lookup x)
  where
    check (s', _) = (||) <$> isInstance s s' <*> isInstance s' s


toMaybe :: Bool -> a -> Maybe a
toMaybe True  = Just
toMaybe False = const Nothing

disjointKeys :: Ord k => Map.Map k v -> Set.Set k -> Bool
disjointKeys m = Map.null . Map.restrictKeys m
