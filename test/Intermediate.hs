{-# LANGUAGE OverloadedStrings #-}
module Intermediate where

import           Compile    (compileIntermediateFile)
import           Internal
import           Test.Hspec


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
