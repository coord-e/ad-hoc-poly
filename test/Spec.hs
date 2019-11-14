{-# LANGUAGE OverloadedStrings #-}

import           Intermediate
import           Source
import           Test.Hspec


main :: IO ()
main = hspec $ do
  testIntermediateSyntax
  testSourceSyntax


