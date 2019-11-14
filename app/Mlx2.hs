module Mlx2 where

import           Compile            (compileSourceFile)
import           Reporting.Report   (printReport)

import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  result <- compileSourceFile Nothing $ head args
  case result of
    Right output -> putStrLn output
    Left err     -> printReport err
