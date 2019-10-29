{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Overload.Kind where

import qualified AST.Source               as S
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH

import qualified Data.Map                 as Map


data Kind
  = Star
  | Constraint
  | Arrow Kind Kind
  deriving Show

makeBaseFunctor ''Kind


type KindEnv = Map.Map S.TypeName Kind

initKindEnv :: KindEnv
initKindEnv = Map.empty


evalKind :: S.Kind -> Kind
evalKind = cata go
  where
    go S.StarF          = Star
    go S.ConstraintF    = Constraint
    go (S.ArrowF k1 k2) = Arrow k1 k2
