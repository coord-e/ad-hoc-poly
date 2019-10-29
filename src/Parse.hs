module Parse where

import           AST.Source
import           Parse.Expr
import           Reporting.Error
import           Reporting.Result

import           Data.Bifunctor
import qualified Text.Megaparsec  as M


parse :: String -> Result Expr
parse = first (ParseError . M.errorBundlePretty) . M.parse expr "input"
