module Reporting.Error where

import           AST.Source           (Type)
import           Compile.Kind         (Kind)

import           Reporting.Error.Kind
import           Reporting.Error.Type

data Error
  = KindError (KindError Kind)
  | TypeError (TypeError Type)
