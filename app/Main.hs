module Main where

import           Compile.Compile (compile)
import           Emit (emit)
import           Parse (parse)
import Reporting.Report (report)

import Control.Monad (<=<)

transpile :: String -> Result String
transpile = fmap emit . compile <=< parse

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ args !! 0
  case transpile content of
    Right output -> putStrLn output
    Left error -> report error
