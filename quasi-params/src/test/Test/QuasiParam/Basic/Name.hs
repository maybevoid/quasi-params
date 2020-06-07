module Test.QuasiParam.Basic.Name where

import Test.Tasty
import Test.Tasty.HUnit

import QuasiParam.Name

tests :: TestTree
tests = testGroup "Quasi parameters basic named parameters tests"
  [ testGetFooBar
  ]

getFooString :: Param "Foo" String => String
getFooString = captureParam @"Foo"

getBarString :: Param "Bar" String => String
getBarString = captureParam @"Bar"

getFooInt :: Param "Foo" Int => Int
getFooInt = captureParam @"Foo"

testGetFooBar :: TestTree
testGetFooBar = testCase "test get foo bar" $
  withParam @"Foo" "foo" $
  withParam @"Bar" "bar" $ do
    assertEqual
      "Should be able to get foo param"
      getFooString "foo"

    assertEqual
      "Should be able to get bar param"
      getBarString "bar"

    assertEqual
      "Should be able to get foo param"
      (captureParam @"Foo") "foo"

    assertEqual
      "Should be able to get bar param"
      (captureParam @"Bar") "bar"
