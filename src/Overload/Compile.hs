module Overload.Compile where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import           Overload.GlobalInfer
import           Overload.Unify
import           Reporting.Error
import           Reporting.Error.Type
import           Reporting.Result

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Monad             (when)


compile :: S.Expr -> Result T.Expr
compile e = do
  ((_, e', left), cs) <- run . runError . runState [] . runReader initEnv . runFresh' 0 $ globalInfer e
  _ <- runSolve cs
  when (not $ null left) (Left $ TypeError $ UnresolvedVariable left)
  return e'
