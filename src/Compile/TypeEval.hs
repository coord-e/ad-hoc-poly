{-# LANGUAGE FlexibleContexts #-}
module Compile.TypeEval where

import qualified AST.Source                as S
import           Compile.Env
import           Compile.Type
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Reader.Strict


eval :: (Member (Reader Env) r, Member (Exc Error) r) => S.Type -> Eff r Type
eval _ = return TInt
