module Reporting.Error.Kind where

import           AST.Source

data KindError a
  = UnificationFail a a
  | UnboundName TypeName

type Result a = Either (KindError a) a


