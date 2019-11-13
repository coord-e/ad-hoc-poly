module Dictionary where

import qualified AST.Intermediate   as T
import qualified AST.Source         as S
import           Dictionary.Convert (runConvert)

import           Reporting.Result   (Result)


compile :: S.Expr -> Result T.Expr
compile = Right . runConvert
