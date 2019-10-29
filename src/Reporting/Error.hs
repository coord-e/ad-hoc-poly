module Reporting.Error where

import           Reporting.Error.Kind
import           Reporting.Error.Type
import           Reporting.Report


data Error
  = KindError KindError
  | TypeError TypeError
  | ParseError String
  | ConfigError String
  deriving Show


instance Report Error where
  report (ParseError s)  = s
  report (KindError e)   = report e
  report (TypeError e)   = report e
  report (ConfigError s) = s
