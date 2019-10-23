module Compile.Compile where

import qualified AST.Source       as S
import qualified AST.Target       as T
import           Reporting.Result

compile :: S.Expr -> Result T.Expr
