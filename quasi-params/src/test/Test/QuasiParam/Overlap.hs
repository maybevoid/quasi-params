module Test.QuasiParam.Overlap where

import Test.Tasty
import Test.Tasty.HUnit

import Data.QuasiParam.Name

tests :: TestTree
tests = testGroup "Quasi parameters overlap tests"
  [ testOverlap
  ]

getOverlap
  :: (Param "Foo" String, Param "Foo" String, Param "Foo" Int)
  => String
getOverlap = (captureParam @"Foo") <> (show $ captureParam @"Foo" @Int)

testOverlap :: TestTree
testOverlap = testCase "test overlapping params" $
  withParam @"Foo" "foo" $
  withParam @"Foo" "bar" $
  withParam @"Foo" (3 :: Int) $ do
    assertEqual "Should be able to get overlapped Foo"
      (captureParam @"Foo")
      "foo"

    assertEqual "Should be able to get overlapped Foo"
      getOverlap
      "foo3"
