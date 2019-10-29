module Reporting.Error.Type where

import qualified AST.Source    as S
import           Overload.Type


data TypeError
  = UnificationFail Type Type
  | InfiniteType TyVar Type
  | UnableToInstantiate TypeScheme TypeScheme
  | UnresolvedVariable [S.Name]
  | UnboundVariable S.Name
  deriving Show
