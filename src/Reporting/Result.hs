module Reporting.Result where

import           Reporting.Error

type Result a = Either a Error
