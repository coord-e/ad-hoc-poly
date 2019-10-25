{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Overload.Type where

import qualified AST.Source               as S

import           Control.Lens.TH
import           Data.Functor.Foldable.TH


type TyVarId = Int

data TyVar
  = Unbound TyVarId Int
  | Bound TyVarId
  deriving Show

-- normalized type (Star kind)
data Type
  = TInt
  | TChar
  | TStr
  | TVar TyVar
  | TFun Type Type
  | TTuple [Type]
  deriving Show

makeBaseFunctor ''Type


-- normalized type (Constraint kind)
data Constraint
  = Constraint { _name        :: S.TypeName
               , _requirement :: Type }
  deriving Show

makeLenses ''Constraint


data TypeScheme
  = Forall { _vars        :: [TyVar],
             _constraints :: [Constraint],
             _type_       :: Type }
  deriving Show

makeLenses ''TypeScheme
