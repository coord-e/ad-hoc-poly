module Main where

import           Emit               (emit)
import           Overload           (compile)
import           Parse              (parse)
import           Reporting.Report   (report)
import           Reporting.Result   (Result)

import           Control.Monad      ((<=<))
import           System.Environment (getArgs)

transpile :: String -> Result String
transpile = fmap emit . compile <=< parse

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ head args
  case transpile content of
    Right output -> putStrLn output
    Left err     -> report err
