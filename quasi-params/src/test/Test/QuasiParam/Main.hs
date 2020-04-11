module Test.QuasiParam.Main (main) where

import Test.Tasty

import qualified Test.QuasiParam.Basic as Basic
import qualified Test.QuasiParam.Overlap as Overlap
import qualified Test.QuasiParam.Stress as Stress

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Quasi parameters tests"
  [ Basic.tests
  , Overlap.tests
  , Stress.tests
  ]
