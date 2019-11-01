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
scanWaitList :: Subst -> WaitList -> (PredType, T.Expr, PSubst) -> Eff '[Writer S.Name, Fresh, Reader Env, State Constraints, Exc Error] (PredType, T.Expr, PSubst)
scanWaitList _ [] acc = return acc
scanWaitList s (Candidate i x p c:wl) (ap, ae, m) = do
  inst <- local (set context c) . findInstantiation x . scheme $ apply s p
  case inst of
    Just (_, e) -> do
      -- TODO: can unification made here be ignored in the scan?
      (p', e', wl') <- raise . local (set context c) $ runLocalInfer e
      _ <- unifyP p p'
      subst <- solve =<< get  -- TODO: save this solve and use later?
      scanWaitList subst (wl ++ wl') (ap, ae, IntMap.insert i e' m)
    Nothing -> do
      n <- freshn x
      tell x
      let (PredType cs t) = ap
      let (PredType cs' t') = p
      let c = Constraint x $ scheme p
      scanWaitList s wl (PredType (c:cs++cs') (TFun t' t), T.Lam n ae, IntMap.insert i (T.Var n) m)

runScanWaitList :: Subst -> PredType -> T.Expr -> WaitList -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] (PredType, T.Expr, PSubst, [S.Name])
runScanWaitList s p e wl = do
  ((p', e', m), left) <- runListWriter $ scanWaitList s wl (p, e, IntMap.empty)
  return (p', e', m, left)

globalInfer :: S.Expr -> Eff '[Fresh, Reader Env, State Constraints, Exc Error] (TypeScheme, TypeScheme, T.Expr, [S.Name])
globalInfer e = do
  (p, e', waitlist) <- runLocalInfer e
  subst <- solve =<< get  -- TODO: save this solve and use later?
  (p', e'', m, left) <- runScanWaitList subst p e' waitlist
  s' <- generalize $ apply subst p'
  s <- generalize $ apply subst p
  return (s', s, resolvePlaceholders m e'', left)

resolvePlaceholders :: PSubst -> T.Expr -> T.Expr
resolvePlaceholders s = cata go
  where
    go :: T.ExprF T.Expr -> T.Expr
    go (T.PlaceholderF i) = cata go $ s IntMap.! i
    go e                  = embed e
