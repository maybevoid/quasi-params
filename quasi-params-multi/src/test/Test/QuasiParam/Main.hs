module Test.QuasiParam.Main (main) where

import Test.Tasty

import qualified Test.QuasiParam.Item as Item

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Quasi parameters tests"
  [ Item.tests
  ]
