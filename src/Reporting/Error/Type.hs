module Reporting.Error.Type where

import qualified AST.Source    as S
import           Overload.Type


data TypeError
  = UnificationFail Type Type
  | InfiniteType TyVar Type
  | UnableToInstantiate TypeScheme TypeScheme
  | UnboundVariable S.Name
  deriving Show
