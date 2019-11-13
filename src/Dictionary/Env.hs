module Dictionary.Env where

import           AST.Name

import qualified Data.Map as Map


type Env = Map.Map String [Name]

initEnv :: Env
initEnv = Map.empty
