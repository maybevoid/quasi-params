{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Test.QuasiParam.Overlap where

import Test.Tasty
import Test.Tasty.HUnit

-- import Data.Kind
import Data.QuasiParam.Name

data Foo

-- class HasConstraint tag where
--   type family ToConstraint tag a = (c :: Constraint) | c -> tag a

--   captureValue :: forall a . (ToConstraint tag a) => a

-- instance HasConstraint Foo where
--   type ToConstraint Foo a = Param "Foo" a

--   captureValue :: forall a . (ToConstraint Foo a) => a
--   captureValue = captureParam @"Foo"

tests :: TestTree
tests = testGroup "Quasi parameters overlap tests"
  [ testOverlap
  ]

-- getOverlap
--   :: (ToConstraint Foo String, ToConstraint Foo Int)
--   -- :: (Param "Foo" String, Param "Foo" String, Param "Foo" Int)
--   => String
-- getOverlap = (captureValue @Foo) <> (show $ captureValue @Foo)

-- getOverlap
--   :: (Param "Foo" String, Param "Foo" String, Param "Foo" Int)
--   => String
-- getOverlap = (captureParam @"Foo") <> (show $ captureParam @"Foo")

testOverlap :: TestTree
testOverlap = testCase "test overlapping params" $
  withParam @"Foo" "foo" $
  withParam @"Foo" "bar" $
  withParam @"Foo" (3 :: Int) $ do
    assertEqual "Should be able to get overlapped Foo"
      (captureParam @"Foo")
      "foo"

    -- assertEqual "Should not able to get overlapped Foo Int"
    --   (captureParam @"Foo")
    --   3

    -- assertEqual "Should be able to get overlapped Foo"
    --   getOverlap
    --   "foo3"
