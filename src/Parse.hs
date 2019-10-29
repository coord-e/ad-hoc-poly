module Parse where

import           AST.Source
import           Reporting.Result


parse :: String -> Result Expr
parse _ = Right (App (Lam "a" $ Var "a") (Int 1))
