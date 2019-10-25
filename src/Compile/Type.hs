{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Compile.Type where

import qualified AST.Source               as S

import           Control.Lens.TH
import           Data.Functor.Foldable.TH


data Type
  = TInt
  | TChar
  | TStr
  | TVar S.TVarName
  | TFun Type Type
  | TLam S.TypeName Type
  | TTuple [Type]
  | TConstraint S.Name Type
  deriving Show

makeBaseFunctor ''Type


data TypeScheme
  = Forall { _vars        :: [S.TVarName],
             _constraints :: [Type],
             _type_       :: Type }
  deriving Show

makeLenses ''TypeScheme
