{-# LANGUAGE DataKinds #-}
module Overload.GlobalInfer where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import           Overload.Instance
import           Overload.LocalInfer
import           Overload.Subst
import           Overload.Type
import           Overload.Unify
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.IntMap               as IntMap


type PSubst = IntMap.IntMap T.Expr


-- TODO: implement without primitive recursion
scanWaitList :: Subst -> WaitList -> ([Constraint], T.Expr, PSubst) -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] ([Constraint], T.Expr, PSubst)
scanWaitList _ [] acc = return acc
scanWaitList subst (Candidate i x t ctx:wl) (acs, ae, m) = do
  inst <- local (set context ctx) . findInstantiationType x $ apply subst t
  case inst of
    Just (_, xt) -> do
      (t', e', wl') <- local (set context ctx) $ runLocalInfer (S.Var xt)
      subst' <- unifyAndSolve t t'
      scanWaitList subst' (wl++wl') (acs, ae, IntMap.insert i e' m)
    Nothing -> do
      n <- freshn x
      let c = Constraint x t
      scanWaitList subst wl (c:acs, T.Lam n ae, IntMap.insert i (T.Var n) m)

runScanWaitList :: Subst -> T.Expr -> WaitList -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] ([Constraint], T.Expr, PSubst)
runScanWaitList s e wl = scanWaitList s wl ([], e, IntMap.empty)


globalInfer :: S.Expr -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] (PredType, T.Expr)
globalInfer e = do
  (t, e', waitlist) <- runLocalInfer e
  subst <- getCurrentSubst
  (cs, e'', m) <- runScanWaitList subst e' waitlist
  return (apply subst (PredType cs t), resolvePlaceholders m e'')

resolvePlaceholders :: PSubst -> T.Expr -> T.Expr
resolvePlaceholders s = cata go
  where
    go :: T.ExprF T.Expr -> T.Expr
    go (T.PlaceholderF i) = cata go $ s IntMap.! i
    go e                  = embed e
