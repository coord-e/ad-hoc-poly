{-# LANGUAGE FlexibleContexts #-}
module Compile.KindInfer where

import           Compile.Env
import           Compile.Kind
import           Compile.Type
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Reader.Strict


kind :: (Member (Reader Env) r, Member (Exc Error) r) => Type -> Eff r Kind
kind _ = return Star
