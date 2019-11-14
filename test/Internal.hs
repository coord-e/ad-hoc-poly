{-# LANGUAGE OverloadedStrings #-}
module Internal where

import           Data.Text        (Text, pack, strip)
import           Shelly           hiding (FilePath)
import           System.IO        (hClose, hPutStr)
import           System.IO.Temp   (withSystemTempFile)
import           Test.Hspec

import           Reporting.Report (report)
import           Reporting.Result (Result)
import           Reporting.Error (Error)


assertRight :: Result a -> a
assertRight (Left err) = error $ report err
assertRight (Right v)  = v

assertLeft :: Show a => Result a -> Error
assertLeft (Left err) = err
assertLeft (Right v)  = error (show v)

runML :: String -> IO Text
runML source = withSystemTempFile ".ml" $ \file h -> do
  hPutStr h =<< readFile "test/data/template.ml"
  hPutStr h source
  hClose h
  out <- shelly . print_stdout False $ run "ocaml" ["-w", "-26", pack file]
  return $ strip out

type Compiler = Maybe FilePath -> FilePath -> IO (Result String)

compileSampleWith :: Compiler -> FilePath -> IO (Result String)
compileSampleWith compiler file = compiler (Just "test/data/env.yaml") ("test/data/" ++ file)

testSampleWith :: Compiler -> FilePath -> IO Text
testSampleWith compiler file = do
  result <- assertRight <$> compileSampleWith compiler file
  runML result

testErrorSampleWith :: Compiler -> FilePath -> IO String
testErrorSampleWith compiler file = report . assertLeft <$> compileSampleWith compiler file

shouldReturnContain :: (HasCallStack, Show a, Eq a) => IO [a] -> [a] -> Expectation
shouldReturnContain a p = flip shouldContain p =<< a

