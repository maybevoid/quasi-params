module QuasiParam.Test.Main (main) where

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Quasi parameters tests"
  []
