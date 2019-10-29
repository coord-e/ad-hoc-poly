{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Overload.Type where

import qualified AST.Source               as S

import           Control.Lens.TH
import           Data.Functor.Foldable.TH
import qualified Data.Map                 as Map


newtype TyVar = TV Int deriving (Show, Eq, Ord)


-- normalized type (Star kind)
data Type
  = TBase String
  | TVar TyVar
  | TFun Type Type
  | TTuple [Type]
  deriving (Show, Eq)

makeBaseFunctor ''Type


-- normalized type (Constraint kind)
data Constraint
  = Constraint { _name        :: S.TypeName
               , _requirement :: TypeScheme }
  deriving (Show, Eq)


data PredType
  = PredType { _constraints :: [Constraint]
             , _type_       :: Type }
  deriving (Show, Eq)


data TypeScheme
  = Forall { _vars     :: [TyVar],
             _predType :: PredType }
  deriving (Show, Eq)


makeLenses ''TypeScheme
makeLenses ''PredType
makeLenses ''Constraint


type TypeEnv = Map.Map S.TypeName PredSem

initTypeEnv :: TypeEnv
initTypeEnv = Map.empty


data Sem
  = SType Type
  | SConstraint Constraint
  | SClosure S.TypeName S.Type TypeEnv

data PredSem
  = PredSem { _constraintsS :: [Constraint]
            , _typeS        :: Sem }

data SemScheme
  = SForall { _varsS    :: [TyVar],
             _predTypeS :: PredSem }

makeLenses ''SemScheme
makeLenses ''PredSem
