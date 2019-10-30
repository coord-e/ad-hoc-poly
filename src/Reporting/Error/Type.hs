module Reporting.Error.Type where

import qualified AST.Source       as S
import           Overload.Type
import           Reporting.Report


data TypeError
  = UnificationFail Type Type
  | InfiniteType TyVar Type
  | UnableToInstantiate S.Name TypeScheme TypeScheme
  | UnresolvedVariable [S.Name]
  | UnboundVariable S.Name
  deriving Show


instance Report TypeError where
  report (UnificationFail t1 t2) = "Couldn't unify expected type " ++ report t1 ++ " with " ++ report t2
  report (InfiniteType tv t) = "Occurs check failed: " ++ report tv ++ " in " ++ report t
  report (UnableToInstantiate x s1 s2) = "Unable to instantiate " ++ show x ++ ": " ++ report s1 ++ " vs " ++ report s2
  report (UnresolvedVariable xs) = "Unresolved variables " ++ show xs
  report (UnboundVariable x) = "Unbound variable " ++ show x
