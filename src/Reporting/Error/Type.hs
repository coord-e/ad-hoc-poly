module Reporting.Error.Type where

import           AST.Source

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVarName Type
  | UnboundVariable Name
