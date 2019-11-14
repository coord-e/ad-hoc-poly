{-# LANGUAGE FlexibleContexts #-}
module Overload.Unify where

import           Overload.Env
import           Overload.Subst
import           Overload.Type
import           Reporting.Error
import           Reporting.Error.Type
import           Reporting.Result

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.State.Strict
import           Control.Lens
import           Control.Monad.Extra      (fromMaybeM)
import           Data.Foldable            (foldrM)
import qualified Data.Set                 as Set
import           Safe.Exact               (zipExactMay)


unify :: Member (State Constraints) r => Type -> Type -> Eff r ()
unify t1 t2 = modify $ over unifications ((t1, t2):)


runSolve :: Constraints -> Either Error Subst
runSolve cs = run . runError $ solve cs

solve :: Member (Exc Error) r => Constraints -> Eff r Subst
solve (Constraints subst cs) = foldrM go subst cs
  where
    go (t1, t2) su = flip compose su <$> unifies (apply su t1) (apply su t2)

getCurrentSubst :: (Member (State Constraints) r, Member (Exc Error) r) => Eff r Subst
getCurrentSubst = do
  s <- solve =<< get
  modify $ set unifications []
  modify $ set solved s
  return s

unifyAndSolve :: (Member (State Constraints) r, Member (Exc Error) r) => Type -> Type -> Eff r Subst
unifyAndSolve t1 t2 = unify t1 t2 >> getCurrentSubst


runUnifies :: Type -> Type -> Result Subst
runUnifies t1 t2 = run . runError $ unifies t1 t2

-- NOTE: left-biased
unifies :: Member (Exc Error) r => Type -> Type -> Eff r Subst
unifies (TBase n1) (TBase n2) | n1 == n2 = return nullSubst
unifies t1@(TFun a1 b1) t2@(TFun a2 b2)  = fromMaybeM (throwUniFail t1 t2) $ unifiesMany [a1, b1] [a2, b2]
unifies t1@(TTuple ts) t2@(TTuple ts')   = fromMaybeM (throwUniFail t1 t2) $ unifiesMany ts ts'
unifies (TVar v) t                       = bind v t
unifies t (TVar v)                       = bind v t
unifies t1 t2                            = throwUniFail t1 t2

unifiesMany :: Member (Exc Error) r => [Type] -> [Type] -> Eff r (Maybe Subst)
unifiesMany ts1 ts2 = mapM (solve . Constraints nullSubst) (zipExactMay ts1 ts2)

bind :: Member (Exc Error) r => TyVar -> Type -> Eff r Subst
bind v t | TVar v == t   = return nullSubst
         | occursIn v t  = throwError . TypeError $ InfiniteType v t
         | otherwise     = return $ singleSubst v t

occursIn :: Substitutable a => TyVar -> a -> Bool
occursIn a t = a `Set.member` ftv t

throwUniFail :: Member (Exc Error) r => Type -> Type -> Eff r a
throwUniFail t1 t2 = throwError . TypeError $ UnificationFail t1 t2
