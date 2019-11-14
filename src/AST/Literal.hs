module AST.Literal where

import           Reporting.Report


data Literal
  = Int Int
  | Char Char
  | Str String
  | Real Double
  | Bool Bool
  deriving (Show, Eq)


-- Report instances
instance Report Literal where
  report (Int i)  = show i
  report (Char c) = show c
  report (Str s)  = show s
  report (Real f) = show f
  report (Bool b) = show b
