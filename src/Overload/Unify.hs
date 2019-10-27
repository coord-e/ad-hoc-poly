{-# LANGUAGE FlexibleContexts #-}
module Overload.Unify where

import           Overload.Subst
import           Overload.Type
import           Reporting.Error

import           Control.Eff
import           Control.Eff.Exception


solve :: Member (Exc Error) r => [(Type, Type)] -> Eff r Subst
solve [] = return nullSubst
solve ((t1, t2) : cs) = do
  s <- unifies t1 t2
  compose s <$> solve (apply s cs)

unifies :: Member (Exc Error) r => Type -> Type -> Eff r Subst
unifies _ _ = return nullSubst
