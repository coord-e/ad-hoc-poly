module Reporting.Error.Kind where

import           AST.Source
import           Compile.Kind

data KindError
  = UnificationFail Kind Kind
  | UnboundName TypeName

type Result a = Either a KindError


