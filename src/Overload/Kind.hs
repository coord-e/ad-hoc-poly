{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Overload.Kind where

import qualified AST.Kind                 as S
import           AST.Name
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Reporting.Report

import qualified Data.Map                 as Map


data Kind
  = Star
  | Constraint
  | Arrow Kind Kind
  deriving Show

makeBaseFunctor ''Kind


type KindEnv = Map.Map TypeName Kind

initKindEnv :: KindEnv
initKindEnv = Map.empty


evalKind :: S.Kind -> Kind
evalKind = cata go
  where
    go S.StarF          = Star
    go S.ConstraintF    = Constraint
    go (S.ArrowF k1 k2) = Arrow k1 k2


-- Report instances
instance Report Kind where
  report = cata go
    where
      go StarF          = "*"
      go ConstraintF    = "C"
      go (ArrowF k1 k2) = paren k1 ++ " -> " ++ k2
