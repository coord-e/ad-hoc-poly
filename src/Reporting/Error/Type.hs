module Reporting.Error.Type where

import           AST.Source

data TypeError a
  = UnificationFail a a
  | InfiniteType TVarName a
  | UnboundVariable Name
