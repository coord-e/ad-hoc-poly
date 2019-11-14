{-# LANGUAGE FlexibleContexts #-}
module Overload.KindInfer where

import qualified AST.Type                  as S
import           Overload.Env
import           Overload.Kind
import           Reporting.Error
import           Reporting.Error.Kind

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Control.Monad             ((<=<))
import           Control.Monad.Extra       (fromMaybeM)
import qualified Data.Map                  as Map


kind :: (Member (Reader Env) r, Member (Exc Error) r) => S.Type -> Eff r Kind
kind (S.TVar _) = return Star
kind (S.TName n) = fromMaybeM (throwKindError $ UnboundName n) $ reader (Map.lookup n . view kindEnv)
kind (S.TFun t1 t2) = do
  k1 <- kind t1
  k2 <- kind t2
  unify Star k1
  unify Star k2
  return Star
kind (S.TApp t1 t2) = do
  k1 <- kind t1
  k2 <- kind t2
  case k1 of
    Arrow a b -> do
      unify Star a
      unify Star k2
      return b
    k -> throwKindError $ UnableToApply k k2
kind (S.TLam x t) = Arrow Star <$> local (over kindEnv $ Map.insert x Star) (kind t)
kind (S.TTuple ts) = mapM_ (unify Star <=< kind) ts >> return Star
kind (S.TConstraint _ t) = (unify Star =<< kind t) >> return Constraint
kind (S.TPredicate t1 t2) = (unify Constraint =<< kind t1) >> kind t2

kindTo :: (Member (Reader Env) r, Member (Exc Error) r) => S.Type -> Kind -> Eff r ()
kindTo t k = unify k =<< kind t


unify :: Member (Exc Error) r => Kind -> Kind -> Eff r ()
unify Star Star                   = return ()
unify Constraint Constraint       = return ()
unify (Arrow a1 b1) (Arrow a2 b2) = unify a1 a2 >> unify b1 b2
unify k1 k2                       = throwKindError $ UnificationFail k1 k2

throwKindError :: Member (Exc Error) r => KindError -> Eff r a
throwKindError = throwError . KindError
