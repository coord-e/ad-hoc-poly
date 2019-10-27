module Reporting.Error.Type where

import qualified AST.Source    as S
import           Overload.Type


data TypeError
  = UnificationFail Type Type
  | InfiniteType S.TVarName Type
  | UnboundVariable S.Name
  deriving Show
