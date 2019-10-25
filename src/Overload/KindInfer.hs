{-# LANGUAGE FlexibleContexts #-}
module Overload.KindInfer where

import           AST.Source
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


kind :: (Member (Reader Env) r, Member (Exc Error) r) => Type -> Eff r Kind
kind TInt = return Star
kind TChar = return Star
kind TStr = return Star
kind (TVar _) = return Star
kind (TName n) = fromMaybeM (throwKindError $ UnboundName n) $ reader (Map.lookup n . view kindEnv)
kind (TFun t1 t2) = do
  k1 <- kind t1
  k2 <- kind t2
  unify Star k1
  unify Star k2
  return Star
kind (TApp t1 t2) = do
  k1 <- kind t1
  k2 <- kind t2
  case k1 of
    Arrow a b -> do
      unify Star a
      unify Star k2
      return b
    k -> throwKindError $ UnableToApply k k2
kind (TLam x t) = Arrow Star <$> local (over kindEnv $ Map.insert x Star) (kind t)
kind (TTuple ts) = mapM_ (unify Star <=< kind) ts >> return Star
kind (TConstraint _ t) = (unify Star =<< kind t) >> return Constraint


unify :: Member (Exc Error) r => Kind -> Kind -> Eff r ()
unify Star Star                   = return ()
unify Constraint Constraint       = return ()
unify (Arrow a1 b1) (Arrow a2 b2) = unify a1 a2 >> unify b1 b2
unify k1 k2                       = throwKindError $ UnificationFail k1 k2

throwKindError :: Member (Exc Error) r => KindError -> Eff r a
throwKindError = throwError . KindError
