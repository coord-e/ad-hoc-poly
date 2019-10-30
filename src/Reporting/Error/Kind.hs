module Reporting.Error.Kind where

import qualified AST.Source       as S
import           Overload.Kind
import           Reporting.Report


data KindError
  = UnificationFail Kind Kind
  | UnableToApply Kind Kind
  | UnboundName S.TypeName
  deriving Show


type Result a = Either KindError a


instance Report KindError where
  report (UnificationFail k1 k2) = "Couldn't unify expected kind " ++ report k1 ++ " with " ++ report k2
  report (UnableToApply k1 k2) = "Unable to apply " ++ report k1 ++ " to " ++ report k2
  report (UnboundName x) = "Unbound type name " ++ show x
