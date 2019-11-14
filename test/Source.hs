{-# LANGUAGE OverloadedStrings #-}
module Source where

import           Compile    (compileSourceFile)
import           Internal
import           Test.Hspec


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
