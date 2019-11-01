module Overload.Subst where

import           Overload.Env          (Context (..))
import           Overload.Type
import           Reporting.Report

import           Control.Exception     (assert)
import           Control.Lens          hiding (Context)
import           Data.Functor.Foldable
import qualified Data.Map              as Map
import qualified Data.Set              as Set


newtype Subst = Subst (Map.Map TyVar Type) deriving Show

nullSubst :: Subst
nullSubst = Subst Map.empty

singleSubst :: TyVar -> Type -> Subst
singleSubst = (Subst .) . Map.singleton

compose :: Subst -> Subst -> Subst
compose s1@(Subst m1) (Subst m2) = Subst $ Map.map (apply s1) m2 `Map.union` m1


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TyVar

instance Substitutable Type where
  apply (Subst s) = cata go
    where
      go t@(TVarF v) = Map.findWithDefault (embed t) v s
      go t           = embed t

  ftv = cata go
    where
      go (TVarF v)     = Set.singleton v
      go (TBaseF _)    = Set.empty
      go (TFunF s1 s2) = s1 `Set.union` s2
      go (TTupleF ss)  = foldr Set.union Set.empty ss

instance Substitutable Constraint where
  apply s (Constraint x t) = Constraint x $ apply s t
  ftv (Constraint _ t) = ftv t

instance Substitutable PredType where
  apply s (PredType cs t) = PredType (apply s cs) (apply s t)
  ftv (PredType cs t) = ftv cs `Set.union` ftv t

instance Substitutable TypeScheme where
  apply (Subst s) (Forall as p) = Forall as $ apply (Subst $ foldr Map.delete s as) p
  ftv (Forall as p) = ftv p `Set.difference` Set.fromList as

instance Substitutable Context where
  apply s (Context overs insts binds) = assert (go1 overs == overs) $
                                        assert (go2 insts == insts) $
                                        Context overs insts (go3 binds)
    where
      go1 = Map.map $ apply s
      go2 = Map.map . map . over _1 $ apply s
      go3 = Map.map . over _1 $ apply s

  ftv (Context overs insts binds) = assert (go1 overs == Set.empty) $
                                    assert (go2 insts == Set.empty) $
                                    go3 binds
    where
      go1 = Map.foldr (Set.union . ftv) Set.empty
      go2 = Map.foldr (Set.union . foldr (Set.union . views _1 ftv) Set.empty) Set.empty
      go3 = Map.foldr (Set.union . views _1 ftv) Set.empty

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply s (a, b) = (apply s a, apply s b)
  ftv (a, b) = ftv a `Set.union` ftv b

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty


-- Report instances
instance Report Subst where
  report (Subst m) = Map.foldrWithKey go "" m
    where
      go v t acc = acc ++ report v ++ " := " ++ report t ++ "\n"
