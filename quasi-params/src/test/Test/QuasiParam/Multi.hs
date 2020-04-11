module Test.QuasiParam.Multi where

import Data.Kind
import Test.Tasty
import Test.Tasty.HUnit

import Data.QuasiParam.Multi

tests :: TestTree
tests = testGroup "Multi parameters test"
  [ testCastFooBar
  ]

data Foo
data Bar

newtype Value a x = Value { unValue :: a }
  deriving Eq

type Item tag a = Elem Type Type tag (Value a)

fooBar :: Union Type (Item Foo String) (Item Bar String) ()
fooBar = Union (Elem $ Value "foo") (Elem $ Value "bar")

barFoo :: Union Type (Item Bar String) (Item Foo String) ()
barFoo = castValue fooBar

testCastFooBar :: TestTree
testCastFooBar = testCase "test cast foo bar" $
  case barFoo of
    Union (Elem (Value bar)) (Elem (Value foo)) -> do
      assertEqual "bar should have bar value"
        bar
        "bar"

      assertEqual "foo should have foo value"
        foo
        "foo"
