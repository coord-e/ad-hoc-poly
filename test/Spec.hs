{-# LANGUAGE OverloadedStrings #-}
import           Data.Text        (Text, pack)
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
  hPutStr h source
  hClose h
  shelly $ run "ocaml" [pack file]

testSample :: FilePath -> IO Text
testSample file = do
  result <- assertRight <$> compileFile (Just "test/data/env.yaml") ("test/data/" ++ file)
  runML result


main :: IO ()
main = hspec $ do
  describe "Overload resolution" $ do
    it "simple example" $ do
      testSample "sample1.mlx1" `shouldReturn` "false"
