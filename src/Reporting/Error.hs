module Reporting.Error where

import           Reporting.Error.Kind
import           Reporting.Error.Type

data Error
  = KindError KindError
  | TypeError TypeError
  deriving Show
