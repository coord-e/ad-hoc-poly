{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module AST.Target where

import           Data.Functor.Foldable.TH

type Name = String

data Expr
  = Int Int
  | Char Char
  | Str String
  | Var Name
  | App Expr Expr
  | Lam Name Expr
  | Tuple [Expr]
  | Let Name Expr Expr
  deriving (Show, Eq)

makeBaseFunctor ''Expr
