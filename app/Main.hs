module Main where

import           Compile            (compileFile)
import           Reporting.Report   (printReport)

import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  result <- compileFile Nothing $ head args
  case result of
    Right output -> putStrLn output
    Left err     -> printReport err
