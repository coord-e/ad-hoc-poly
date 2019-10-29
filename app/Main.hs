module Main where

import           Config             (Config, loadDefaultConfigFile)
import           Emit               (emit)
import           Overload           (compile)
import           Parse              (parse)
import           Reporting.Report   (printReport)
import           Reporting.Result   (Result)

import           Control.Monad      ((<=<))
import           Data.Text          hiding (head)
import qualified Data.Text.IO       as T
import           System.Environment (getArgs)


transpile :: Config -> Text -> Result String
transpile c = fmap emit . compile c <=< parse

main :: IO ()
main = do
  args <- getArgs
  content <- T.readFile $ head args
  config <- loadDefaultConfigFile
  case config >>= flip transpile content of
    Right output -> putStrLn output
    Left err     -> printReport err
