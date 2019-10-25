{-# LANGUAGE TemplateHaskell #-}
module Compile.Env where

import qualified AST.Source      as S
import qualified AST.Target      as T
import           Compile.Kind
import           Compile.Type

import           Control.Lens.TH
import qualified Data.Map        as Map


type KindEnv = Map.Map S.TypeName Kind

initKindEnv :: KindEnv
initKindEnv = Map.fromList [("Int", Star), ("Char", Star), ("String", Star)]


type TypeEnv = Map.Map S.TypeName Type

initTypeEnv :: TypeEnv
initTypeEnv = Map.fromList [("Int", TInt), ("Char", TChar), ("String", TStr)]


data Context
  = Context { _instantiations :: Map.Map S.Name (TypeScheme, T.Expr)
            , _overloads      :: Map.Map S.Name TypeScheme
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


newtype Unique = Unique Int

initUnique :: Unique
initUnique = Unique 0


type Constraint = (Type, Type)

data Infer
  = Infer { _unique      :: Unique
          , _constraints :: [Constraint] }

makeLenses ''Infer

initInfer :: Infer
initInfer = Infer initUnique []
