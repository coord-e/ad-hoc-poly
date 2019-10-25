module Compile.Kind where

import           AST.Source
import           Reporting.Error.Kind

kind :: Expr -> Result Kind
kind _ = Right Star
