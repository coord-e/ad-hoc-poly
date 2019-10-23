module Compile.Kind where

import           AST.Source
import           Reporting.Error.Kind

data Kind
  = Type
  | Constraint
  | Arrow Kind Kind

kind :: Expr -> Result Kind
