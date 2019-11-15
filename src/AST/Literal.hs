{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AST.Literal where

import           Reporting.Report

import           Data.Data
import           Data.Ix


data Literal
  = Int Int
  | Char Char
  | Str String
  | Real Double
  | Bool Bool
  deriving (Show, Eq, Data)


newtype LiteralKind = LiteralKind Int deriving (Ix, Eq, Ord)

toKind :: Literal -> LiteralKind
toKind l = LiteralKind i
  where
    AlgConstr i = constrRep (toConstr l)

litK :: (a -> Literal) -> LiteralKind
litK c = toKind $ c (undefined :: a)


-- Report instances
instance Report Literal where
  report (Int i)  = show i
  report (Char c) = show c
  report (Str s)  = show s
  report (Real f) = show f
  report (Bool b) = show b
