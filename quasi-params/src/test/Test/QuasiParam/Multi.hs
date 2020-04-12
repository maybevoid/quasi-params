module Test.QuasiParam.Multi where

import Test.Tasty

import qualified Test.QuasiParam.Multi.Item as Item
import qualified Test.QuasiParam.Multi.Bifunctor as Bifunctor

tests :: TestTree
tests = testGroup "Multi parameters tests"
  [ Item.tests
  , Bifunctor.tests
  ]
