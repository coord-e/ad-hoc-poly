{-# LANGUAGE FlexibleContexts #-}
module Overload.Instance where

import           AST.Name
import           Overload.Env
import           Overload.Internal
import           Overload.Subst
import           Overload.Type
import           Overload.Unify
import           Reporting.Error
import           Reporting.Error.Type

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Control.Monad.Extra       (allM, filterM)
import           Data.Either.Extra         (eitherToMaybe)
import qualified Data.Map                  as Map
import           Data.Maybe                (isJust)
import qualified Data.Set                  as Set


-- isInstance a b returns True if b is more general than a.
isInstance :: (Member (Exc Error) r, Member (Reader Env) r) => TypeScheme -> TypeScheme -> Eff r Bool
isInstance (Forall as1 p1) t2@(Forall _as2 p2) = (&& Set.disjoint (ftv t2) (Set.fromList as1)) <$> isInstancePred p1 p2

isInstancePred :: (Member (Exc Error) r, Member (Reader Env) r) => PredType -> PredType -> Eff r Bool
isInstancePred (PredType cs1 t1) (PredType cs2 t2) = maybe (return False) checkAll $ isInstanceType t1 t2
  where
    check cs c = (|| c `elem` cs) <$> canBeEliminated c
    checkAll s = allM (check $ apply s cs1) (apply s cs2)

isInstanceType :: Type -> Type -> Maybe Subst
isInstanceType t1 t2 = do
  s@(Subst m) <- eitherToMaybe $ runUnifies t2 t1
  toMaybe (disjointKeys m $ ftv t1) s

findInstantiation :: (Member (Exc Error) r, Member (Reader Env) r) => Name -> TypeScheme -> Eff r (Maybe (TypeScheme, Name))
findInstantiation x s = check =<< mapM (filterM . views _1 $ isInstance s) =<< reader (views (context . instantiations) $ Map.lookup x)
  where
    check (Just [inst]) = return $ Just inst
    check (Just [])     = return Nothing
    check (Just _)      = throwError . TypeError $ OverlappingInstance x s
    check Nothing       = return Nothing

findInstantiationType :: (Member (Exc Error) r, Member (Reader Env) r) => Name -> Type -> Eff r (Maybe (TypeScheme, Name))
findInstantiationType x = findInstantiation x . scheme

canBeEliminated :: (Member (Exc Error) r, Member (Reader Env) r) => Constraint -> Eff r Bool
canBeEliminated (Constraint x t) = (||) <$> bound <*> inst
  where
    s = scheme t
    bound = (== Just s) <$> reader (views (context . bindings) $ Map.lookup x)
    inst = isJust <$> findInstantiation x s
