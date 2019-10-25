module Overload.Compile where

import qualified AST.Source       as S
import qualified AST.Target       as T
import           Reporting.Result

compile :: S.Expr -> Result T.Expr
compile _ = Right (T.App (T.Lam "a" $ T.Var "a") (T.Int 1))
