{-# LANGUAGE FlexibleContexts #-}
module Overload.Unify where

import           Overload.Env
import           Overload.Subst
import           Overload.Type
import           Reporting.Error
import           Reporting.Error.Type

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.State.Strict
import           Control.Monad.Extra      (fromMaybeM)
import qualified Data.Set                 as Set
import           Safe.Exact               (zipExactMay)


unify :: Member (State Constraints) r => Type -> Type -> Eff r ()
unify t1 t2 = modify ((t1, t2):)

unifyP :: Member (State Constraints) r => PredType -> PredType -> Eff r PredType
unifyP (PredType cs1 t1) (PredType cs2 t2) = unify t1 t2 >> return (PredType (cs1 ++ cs2) t1)


solve :: Member (Exc Error) r => [(Type, Type)] -> Eff r Subst
solve [] = return nullSubst
solve ((t1, t2) : cs) = do
  s <- unifies t1 t2
  compose s <$> solve (apply s cs)

unifies :: Member (Exc Error) r => Type -> Type -> Eff r Subst
unifies TInt TInt                       = return nullSubst
unifies TChar TChar                     = return nullSubst
unifies TStr TStr                       = return nullSubst
unifies t1@(TFun a1 b1) t2@(TFun a2 b2) = fromMaybeM (throwUniFail t1 t2) $ unifiesMany [a1, b1] [a2, b2]
unifies t1@(TTuple ts) t2@(TTuple ts')  = fromMaybeM (throwUniFail t1 t2) $ unifiesMany ts ts'
unifies (TVar v) t                      = bind v t
unifies t (TVar v)                      = bind v t
unifies t1 t2                           = throwUniFail t1 t2

unifiesMany :: Member (Exc Error) r => [Type] -> [Type] -> Eff r (Maybe Subst)
unifiesMany ts1 ts2 = mapM solve (zipExactMay ts1 ts2)

bind :: Member (Exc Error) r => TyVar -> Type -> Eff r Subst
bind v t | (TVar v) == t = return nullSubst
         | occursIn v t  = throwError . TypeError $ InfiniteType v t
         | otherwise     = return $ singleSubst v t

occursIn :: Substitutable a => TyVar -> a -> Bool
occursIn a t = a `Set.member` ftv t

throwUniFail :: Member (Exc Error) r => Type -> Type -> Eff r a
throwUniFail t1 t2 = throwError . TypeError $ UnificationFail t1 t2
