module Test.QuasiParam.Item (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.QuasiParam.Item.Internal
import qualified Data.QuasiParam.Name as Name

tests :: TestTree
tests = testGroup "Multi parameters unitype items test"
  [ testConversion
  ]

data Baz a = Baz a

instance MultiParam Baz where
  type ParamConstraint Baz a
    = Name.Param "Baz" (Baz a)

  withParam = Name.withParam @"Baz"
  captureParam = Name.captureParam @"Baz"

type Foo = Item "Foo"
type Bar = Item "Bar"

type FooBar = Items '[ Foo, Bar ]
type FooBaz = Items '[ Foo, Baz ]
type FooBarBaz = Items '[ Foo, Bar, Baz ]

type BarFoo = Items '[ Bar, Foo ]
type BazBarFoo = Items '[ Baz, Bar, Foo ]

foo :: Foo String
foo = Item "foo"

bar :: Bar String
bar = Item "bar"

baz :: Baz String
baz = Baz "baz"

fooBar :: FooBar String
fooBar = foo :+ bar

fooBarBaz :: FooBarBaz String
fooBarBaz = foo :+ bar :+ baz

barFoo :: BarFoo String
barFoo = castValue fooBar

bazBarFoo :: BazBarFoo String
bazBarFoo = castValue fooBarBaz

fooBaz :: FooBaz String
fooBaz = castValue bazBarFoo

testConversion :: TestTree
testConversion = testCase "test conversion to multi param" $ do
  case barFoo of
    Item bar2 :+ Item foo2 ->
      assertEqual
        "foo bar should be converted to bar foo"
        (bar2, foo2)
        ("bar", "foo")

  case bazBarFoo of
    Baz baz2 :+ Item bar2 :+ Item foo2 ->
      assertEqual
        "foo bar should be converted to bar foo"
        (baz2, (bar2, foo2))
        ("baz", ("bar", "foo"))

  case fooBaz of
    Item foo2 :+ Baz baz2 ->
      assertEqual
        "foo bar should be converted to foo baz"
        (foo2, baz2)
        ("foo", "baz")
