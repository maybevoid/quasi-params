module Test.QuasiParam.Basic where

import Test.Tasty

import qualified Test.QuasiParam.Basic.Name as Name

tests :: TestTree
tests = testGroup "Quasi parameters basic tests"
  [ Name.tests
  ]
