module Reporting.Result where

import           Reporting.Error

type Result a = Either Error a
