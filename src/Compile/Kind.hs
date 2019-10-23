module Compile.Kind where

import           AST.Source
import           Reporting.Error.Kind

data Kind
  = Star
  | Constraint
  | Arrow Kind Kind

kind :: Expr -> Result Kind
kind _ = Right Star
