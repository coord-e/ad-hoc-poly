module Overload.Subst where

import           Overload.Type

import           Data.Functor.Foldable
import qualified Data.Map              as Map
import qualified Data.Set              as Set


newtype Subst = Subst (Map.Map TyVar Type) deriving Show

nullSubst :: Subst
nullSubst = Subst Map.empty


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
      go TIntF         = Set.empty
      go TCharF        = Set.empty
      go TStrF         = Set.empty
      go (TFunF s1 s2) = s1 `Set.union` s2
      go (TTupleF ss)  = foldr Set.union Set.empty ss

instance Substitutable TypeScheme where
  apply (Subst s) (Forall as cs t) = Forall as cs $ apply (Subst $ foldr Map.delete s as) t
  ftv (Forall as _ t) = ftv t `Set.difference` Set.fromList as

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply s (a, b) = (apply s a, apply s b)
  ftv (a, b) = ftv a `Set.union` ftv b

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty
