{-# LANGUAGE DataKinds #-}
module Overload.GlobalInfer where

import qualified AST.Source                as S
import qualified AST.Target                as T
import           Overload.Env
import           Overload.Type
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Fresh
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict


globalInfer :: S.Expr -> Eff '[Exc Error, Fresh, Reader Env, State Constraints] (PredType, T.Expr, [S.Name])