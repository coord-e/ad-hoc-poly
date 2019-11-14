{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Kind where

import           Reporting.Report

import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH


data Kind
  = Star
  | Constraint
  | Arrow Kind Kind
  deriving Show

makeBaseFunctor ''Kind


-- Report instances
instance Report Kind where
  report = cata go
    where
      go StarF          = "*"
      go ConstraintF    = "C"
      go (ArrowF k1 k2) = paren k1 ++ " -> " ++ k2
