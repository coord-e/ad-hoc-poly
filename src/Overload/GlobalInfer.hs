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
import           Overload.Var
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Extend
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Eff.Writer.Strict
import           Control.Lens
import           Data.Functor.Foldable
import qualified Data.IntMap               as IntMap


type PSubst = IntMap.IntMap T.Expr


-- TODO: implement without primitive recursion
scanWaitList :: Subst -> WaitList -> ([Constraint], Type, T.Expr, PSubst) -> Eff '[Writer S.Name, Fresh, Reader Env, State Constraints, Exc Error] ([Constraint], Type, T.Expr, PSubst)
scanWaitList _ [] acc = return acc
scanWaitList s (Candidate i x p ctx:wl) (acs, aty, ae, m) = do
  inst <- local (set context ctx) . findInstantiation x . scheme $ apply s p
  case inst of
    Just (_, e) -> do
      (p', e', wl') <- raise . local (set context ctx) $ runLocalInfer e
      _ <- unifyP p p'
      subst <- solve =<< get  -- TODO: save this solve and use later?
      scanWaitList subst (wl ++ wl') (acs, aty, ae, IntMap.insert i e' m)
    Nothing -> do
      n <- freshn x
      tell x
      let (PredType cs t) = p
      let c = Constraint x $ scheme p
      scanWaitList s wl (c:cs++acs, (TFun t aty), T.Lam n ae, IntMap.insert i (T.Var n) m)

runScanWaitList :: Subst -> PredType -> T.Expr -> WaitList -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] ([Constraint], PredType, T.Expr, PSubst, [S.Name])
runScanWaitList s (PredType cs t) e wl = do
  ((cs', t', e', m), left) <- runListWriter $ scanWaitList s wl (cs, t, e, IntMap.empty)
  return (cs', PredType cs' t', e', m, left)

globalInfer :: S.Expr -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] (TypeScheme, TypeScheme, T.Expr, [S.Name])
globalInfer e = do
  (p, e', waitlist) <- runLocalInfer e
  subst <- solve =<< get  -- TODO: save this solve and use later?
  (cs, p', e'', m, left) <- runScanWaitList subst p e' waitlist
  s' <- generalize $ apply subst p'
  s <- generalize . apply subst $ addpred cs p
  return (s', s, resolvePlaceholders m e'', left)

resolvePlaceholders :: PSubst -> T.Expr -> T.Expr
resolvePlaceholders s = cata go
  where
    go :: T.ExprF T.Expr -> T.Expr
    go (T.PlaceholderF i) = cata go $ s IntMap.! i
    go e                  = embed e
