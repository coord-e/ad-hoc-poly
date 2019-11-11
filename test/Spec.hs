{-# LANGUAGE OverloadedStrings #-}
import           Data.Text        (Text, pack, strip)
import           Shelly           hiding (FilePath)
import           System.IO        (hClose, hPutStr)
import           System.IO.Temp   (withSystemTempFile)
import           Test.Hspec

import           Compile          (compileFile)
import           Reporting.Report (report)
import           Reporting.Result (Result)


assertRight :: Result a -> a
assertRight (Left err) = error $ report err
assertRight (Right v)  = v

runML :: String -> IO Text
runML source = withSystemTempFile ".ml" $ \file h -> do
  hPutStr h =<< readFile "test/data/template.ml"
  hPutStr h source
  hClose h
  out <- shelly . print_stdout False $ run "ocaml" ["-w", "-26", pack file]
  return $ strip out

testSample :: FilePath -> IO Text
testSample file = do
  result <- assertRight <$> compileFile (Just "test/data/env.yaml") ("test/data/" ++ file)
  runML result


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
