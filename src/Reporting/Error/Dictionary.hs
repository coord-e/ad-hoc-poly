module Reporting.Error.Dictionary where

import           AST.Name
import           Reporting.Report


data DictionaryError
  = MissingImpl TypeName Name
  | UndeclaredClass TypeName
  deriving Show


instance Report DictionaryError where
  report (MissingImpl c ns) = "Missing implementation of class " ++ show c ++ ": " ++ show ns
  report (UndeclaredClass c) = "Cannot find class " ++ show c
