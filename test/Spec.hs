{-# LANGUAGE OverloadedStrings #-}
import           Data.Text        (Text, pack, strip)
import           Shelly           hiding (FilePath)
import           System.IO        (hClose, hPutStr)
import           System.IO.Temp   (withSystemTempFile)
import           Test.Hspec

import           Compile          (compileIntermediateFile, compileSourceFile)
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


main :: IO ()
main = hspec $ do
  testIntermediateSyntax
  testSourceSyntax


testIntermediateSyntax :: SpecWith ()
testIntermediateSyntax =
  describe "Overload resolution" $ do
    it "handle constrainted instantiations" $
      testSample "equality.mlx1" `shouldReturn` "false"

    it "handle multi-parameter constraints" $
      testSample "into.mlx1" `shouldReturn` "42"

    it "handle dictionaries" $
      testSample "number.mlx1" `shouldReturn` "13"

    it "handle superclasses" $
      testSample "superclass.mlx1" `shouldReturn` "true"

    it "fails without necessary predicates" $
      testErrorSample "equality_err.mlx1" `shouldReturnContain` "Unable to instantiate"

    it "fails with overlapping instances" $
      testErrorSample "overlap_err.mlx1" `shouldReturnContain` "overlapping"

    it "fails with unmet superclass" $
      testErrorSample "superclass_err.mlx1" `shouldReturnContain` "Unresolved constraint"
  where
    testSample = testSampleWith compileIntermediateFile
    testErrorSample = testErrorSampleWith compileIntermediateFile


testSourceSyntax :: SpecWith ()
testSourceSyntax =
  describe "Class syntax" $ do
    it "handle constrainted instantiations" $
      testSample "equality.mlx2" `shouldReturn` "false"

    it "handle multi-parameter constraints" $
      testSample "into.mlx2" `shouldReturn` "42"

    it "handle dictionaries" $
      testSample "number.mlx2" `shouldReturn` "13"

    it "handle superclasses" $
      testSample "superclass.mlx2" `shouldReturn` "true"
  where
    testSample = testSampleWith compileSourceFile
