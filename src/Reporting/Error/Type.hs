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
  report (UnificationFail t1 t2) = "Couldn't unify expected type " ++ show t1 ++ " with " ++ show t2
  report (InfiniteType tv t) = "Occurs check failed: " ++ show tv ++ " in " ++ show t
  report (UnableToInstantiate x s1 s2) = "Unable to instantiate " ++ show x ++ ": " ++ show s1 ++ " vs " ++ show s2
  report (UnresolvedVariable xs) = "Unresolved variables " ++ show xs
  report (UnboundVariable x) = "Unbound variable " ++ show x
