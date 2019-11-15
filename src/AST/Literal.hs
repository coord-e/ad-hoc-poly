{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AST.Literal where

import           Reporting.Report

import           Data.Array
import           Data.Data


data Literal
  = Int Int
  | Char Char
  | Str String
  | Real Double
  | Bool Bool
  deriving (Show, Eq, Data)


newtype LiteralKind = LiteralKind Int deriving (Ix, Eq, Ord)

instance Bounded LiteralKind where
  minBound = LiteralKind 1
  maxBound = LiteralKind . maxConstrIndex . dataTypeOf $ (undefined :: Literal)

toKind :: Literal -> LiteralKind
toKind l = LiteralKind i
  where
    AlgConstr i = constrRep (toConstr l)

litK :: (a -> Literal) -> LiteralKind
litK c = toKind $ c (undefined :: a)

type MapLit a = Array LiteralKind a


-- Report instances
instance Report Literal where
  report (Int i)  = show i
  report (Char c) = show c
  report (Str s)  = show s
  report (Real f) = show f
  report (Bool b) = show b
