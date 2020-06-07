{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Test.QuasiParam.Overlap where

import Test.Tasty
import Test.Tasty.HUnit

-- import Data.Kind
import QuasiParam.Name

data Foo

tests :: TestTree
tests = testGroup "Quasi parameters overlap tests"
  [ testOverlap
  ]

testOverlap :: TestTree
testOverlap = testCase "test overlapping params" $
  withParam @"Foo" "foo" $
  withParam @"Foo" "bar" $
  withParam @"Foo" (3 :: Int) $ do
    assertEqual "Should be able to get overlapped Foo"
      (captureParam @"Foo")
      "foo"
