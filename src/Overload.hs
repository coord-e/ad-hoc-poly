{-# LANGUAGE FlexibleContexts #-}
module Overload where

import qualified AST.Intermediate          as S
import qualified AST.Kind                  as S
import           AST.Literal
import qualified AST.Target                as T
import qualified AST.Type                  as S
import           Config                    hiding (literalTypes)
import           Overload.Env
import           Overload.GlobalInfer
import           Overload.Kind             (Kind (..), evalKind)
import           Overload.KindInfer        (kindTo)
import           Overload.LocalInfer       (withBinding)
import           Overload.Type
import           Overload.TypeEval
import           Overload.Unify
import           Reporting.Error
import           Reporting.Error.Type
import           Reporting.Result

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Exception         (assert)
import           Control.Lens
import           Control.Monad             (unless)
import qualified Data.Map                  as Map


compile :: Config -> S.Expr -> Result T.Expr
compile (Config bases lits binds) e = do
  ((PredType cstrs _, e'), cs) <- run
                                  . runError
                                  . runState initConstraints
                                  . runReader (mkInitEnv bases)
                                  . runFresh' 0
                                  . loadLitTypes lits
                                  . loadBindings binds
                                  $ globalInfer e
  _ <- runSolve cs
  unless (null cstrs) (Left . TypeError $ UnresolvedVariable cstrs)
  return e'


-- NOTE: `literalTypes` in `Env` is left undefined here and is initialized in `loadLitTypes`
mkInitEnv :: Map.Map String S.Kind -> Env
mkInitEnv m = Env initContext kindenv typeenv undefined
  where
    kindenv = Map.map evalKind m
    typeenv = Map.mapWithKey go m
    go = const . PredSem [] . SType . TBase

loadLitTypes :: (Member Fresh r, Member (Reader Env) r, Member (Exc Error) r) => MapLit S.Type -> Eff r a -> Eff r a
loadLitTypes lits m = do
  lits' <- mapM go lits
  local (set literalTypes lits') m
  where
    go t = do
      kindTo t Star
      PredType cs t' <- runEvalToType t
      assert (null cs) $ return t'

loadBindings :: (Member Fresh r, Member (Reader Env) r) => Map.Map String S.TypeScheme -> Eff r a -> Eff r a
loadBindings = flip $ Map.foldrWithKey go
  where
    go x s e = do
      s' <- runSchemeEvalToType s
      withBinding x s' e
