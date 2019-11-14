{-# LANGUAGE TemplateHaskell #-}
module Overload.Env where

import           AST.Name
import           Config
import           Overload.Kind
import           Overload.Subst
import           Overload.Type
import           Reporting.Report

import           Control.Exception (assert)
import           Control.Lens      hiding (Context)
import qualified Data.Map          as Map
import qualified Data.Set          as Set


data Context
  = Context { _overloads      :: Map.Map Name TypeScheme
            , _instantiations :: Map.Map Name [(TypeScheme, Name)]
            , _bindings       :: Map.Map Name TypeScheme }

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
              , _name         :: Name
              , _type_        :: Type
              , _savedContext :: Context }

makeLenses ''Candidate

type WaitList = [Candidate]


data Constraints
  = Constraints { _solved       :: Subst
                , _unifications :: [(Type, Type)] }

makeLenses ''Constraints

initConstraints :: Constraints
initConstraints = Constraints nullSubst []


-- Report instances
instance Report Context where
  report (Context overs insts binds) = "[overloadings]\n" ++ go1 overs ++ "[instantiations]\n" ++ go2 insts ++ "[bindings]\n" ++ go1 binds
    where
      go1 = Map.foldrWithKey (\x s acc -> acc ++ x ++ " = " ++ report s ++ "\n") ""
      go2 = Map.foldrWithKey (\x is acc -> acc ++ foldr (folder x) "" is) ""
      folder x (s, xt) acc = acc ++ x ++ " = " ++ report s ++ " ~> " ++ xt ++ "\n"


-- Substitutable instances
instance Substitutable Context where
  apply s (Context overs insts binds) = assert (go1 overs == overs) $
                                        assert (go2 insts == insts) $
                                        Context overs insts (go1 binds)
    where
      go1 = Map.map $ apply s
      go2 = Map.map . map . over _1 $ apply s

  ftv (Context overs insts binds) = assert (go1 overs == Set.empty) $
                                    assert (go2 insts == Set.empty) $
                                    go1 binds
    where
      go1 = Map.foldr (Set.union . ftv) Set.empty
      go2 = Map.foldr (Set.union . foldr (Set.union . views _1 ftv) Set.empty) Set.empty
