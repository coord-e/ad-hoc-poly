{-# LANGUAGE TemplateHaskell #-}
module Overload.Env where

import qualified AST.Source      as S
import           Config
import           Overload.Kind
import           Overload.Type

import           Control.Lens.TH
import qualified Data.Map        as Map


data Context
  = Context { _overloads      :: Map.Map S.Name TypeScheme
            , _instantiations :: Map.Map S.Name [(TypeScheme, S.Expr)]
            , _bindings       :: Map.Map S.Name (TypeScheme, S.Expr) }

makeLenses ''Context

initContext :: Context
initContext = Context Map.empty Map.empty Map.empty


data Env
  = Env { _context      :: Context
        , _kindEnv      :: KindEnv
        , _typeEnv      :: TypeEnv
        , _literalTypes :: LiteralTypes }

makeLenses ''Env


data Candidate
  = Candidate { _id_   :: Int
              , _name  :: S.Name
              , _type_ :: PredType }

makeLenses ''Candidate

type WaitList = [Candidate]


type Constraints = [(Type, Type)]
