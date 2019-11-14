module Mlx1 where

import           Compile            (compileIntermediateFile)
import           Reporting.Report   (printReport)

import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  result <- compileIntermediateFile Nothing $ head args
  case result of
    Right output -> putStrLn output
    Left err     -> printReport err
