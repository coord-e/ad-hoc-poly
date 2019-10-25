module Reporting.Error.Kind where

import           AST.Source

data KindError
  = UnificationFail Kind Kind
  | UnableToApply Kind Kind
  | UnboundName TypeName

type Result a = Either KindError a


