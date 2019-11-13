{-# LANGUAGE OverloadedStrings #-}
import           Data.Text        (Text, pack, strip)
import           Shelly           hiding (FilePath)
import           System.IO        (hClose, hPutStr)
import           System.IO.Temp   (withSystemTempFile)
import           Test.Hspec

import           Compile          (compileFile)
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

compileFile' :: FilePath -> IO (Result String)
compileFile' file = compileFile (Just "test/data/env.yaml") ("test/data/" ++ file)

testSample :: FilePath -> IO Text
testSample file = do
  result <- assertRight <$> compileFile' file
  runML result

testErrorSample :: FilePath -> IO String
testErrorSample file = report . assertLeft <$> compileFile' file

shouldReturnContain :: (HasCallStack, Show a, Eq a) => IO [a] -> [a] -> Expectation
shouldReturnContain a p = flip shouldContain p =<< a


main :: IO ()
main = hspec $ do
  describe "Overload resolution" $ do
    it "handle constrainted instantiations" $ do
      testSample "equality.mlx1" `shouldReturn` "false"

    it "handle multi-parameter constraints" $ do
      testSample "into.mlx1" `shouldReturn` "42"

    it "handle dictionaries" $ do
      testSample "number.mlx1" `shouldReturn` "13"

    it "handle superclasses" $ do
      testSample "superclass.mlx1" `shouldReturn` "true"

    it "fails without necessary predicates" $ do
      testErrorSample "equality_err.mlx1" `shouldReturnContain` "Unable to instantiate"

    it "fails with overlapping instances" $ do
      testErrorSample "overlap_err.mlx1" `shouldReturnContain` "overlapping"

    it "fails with unmet superclass" $ do
      testErrorSample "superclass_err.mlx1" `shouldReturnContain` "Unresolved constraint"
