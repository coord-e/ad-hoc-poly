{-# LANGUAGE TemplateHaskell #-}
module Overload.Env where

import qualified AST.Source       as S
import           Config
import           Overload.Kind
import           Overload.Type
import           Reporting.Report

import           Control.Lens.TH
import qualified Data.Map         as Map


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
  = Candidate { _id_          :: Int
              , _name         :: S.Name
              , _type_        :: PredType
              , _savedContext :: Context }

makeLenses ''Candidate

type WaitList = [Candidate]


type Constraints = [(Type, Type)]


-- Report instances
instance Report Context where
  report (Context overs insts binds) = "[overloadings]\n" ++ go1 overs ++ "[instantiations]\n" ++ go2 insts ++ "[bindings]\n" ++ go3 binds
    where
      go1 = Map.foldrWithKey (\x s acc -> acc ++ x ++ " = " ++ report s ++ "\n") ""
      go2 = Map.foldrWithKey (\x is acc -> acc ++ foldr (folder x) "" is) ""
      folder x (s, e) acc = acc ++ x ++ " = " ++ report s ++ " ~> " ++ report e ++ "\n"
      go3 = Map.foldrWithKey (\x (s, e) acc -> acc ++ x ++ " = " ++ report s ++ " ~> " ++ report e ++ "\n") ""
