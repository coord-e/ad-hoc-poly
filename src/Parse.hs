module Parse where

import qualified AST.Source       as S
import           Reporting.Result

parse :: String -> Result S.Expr
