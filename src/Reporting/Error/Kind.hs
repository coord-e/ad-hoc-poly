module Reporting.Error.Kind where

import qualified AST.Source    as S
import           Overload.Kind


data KindError
  = UnificationFail Kind Kind
  | UnableToApply Kind Kind
  | UnboundName S.TypeName
  deriving Show


type Result a = Either KindError a


