{-# LANGUAGE TemplateHaskell #-}
module Overload.Env where

import qualified AST.Source      as S
import qualified AST.Target      as T
import           Overload.Kind
import           Overload.Type

import           Control.Lens.TH
import qualified Data.Map        as Map


type KindEnv = Map.Map S.TypeName Kind

initKindEnv :: KindEnv
initKindEnv = Map.empty


type TypeEnv = Map.Map S.TypeName S.Type

initTypeEnv :: TypeEnv
initTypeEnv = Map.empty


data Context
  = Context { _overloads      :: Map.Map S.Name TypeScheme
            , _instantiations :: Map.Map S.Name (TypeScheme, T.Expr)
            , _bindings       :: Map.Map S.Name (TypeScheme, T.Expr) }

makeLenses ''Context

initContext :: Context
initContext = Context Map.empty Map.empty Map.empty


data Env
  = Env { _context :: Context
        , _kindEnv :: KindEnv
        , _typeEnv :: TypeEnv }

makeLenses ''Env

initEnv :: Env
initEnv = Env initContext initKindEnv initTypeEnv


data Candidate
  = Candidate { _name  :: S.Name
              , _type_ :: TypeScheme }

makeLenses ''Candidate

type WaitList = [Candidate]


data Infer
  = Infer { _unique      :: Int
          , _constraints :: [(Type, Type)] }

makeLenses ''Infer

initInfer :: Infer
initInfer = Infer 0 []
