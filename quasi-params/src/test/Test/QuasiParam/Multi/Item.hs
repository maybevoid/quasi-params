module Test.QuasiParam.Multi.Item (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Kind
import GHC.Types (Symbol)
import Data.Functor.Identity

import Test.QuasiParam.Multi.Internal.Item
import qualified QuasiParam.Multi as Multi

tests :: TestTree
tests = testGroup "Multi parameters unitype items test"
  [ testConversion
  ]

type Foo = Item "Foo"
type Bar = Item "Bar"
type Baz = Item "Baz"

type FooBar = Cons Foo Bar
type FooBaz = Cons Foo Baz
type FooBarBaz = Cons Foo (Cons Bar Baz)

type BarFoo = Cons Bar Foo
type BazBarFoo = Cons Baz (Cons Bar Foo)

type Foo' = Multi.Elem Symbol Type "Foo" Identity
type Bar' = Multi.Elem Symbol Type "Bar" Identity
type FooBar' = Multi.Cons Type Foo' Bar'

pattern Foo :: forall a . a -> Item "Foo" a
pattern Foo a = Item a
{-# COMPLETE Foo #-}

pattern Bar :: forall a . a -> Item "Bar" a
pattern Bar a = Item a
{-# COMPLETE Bar #-}

pattern Baz :: forall a . a -> Item "Baz" a
pattern Baz a = Item a
{-# COMPLETE Baz #-}

pattern Cons :: forall e1 e2 a . e1 a -> e2 a -> Cons e1 e2 a
pattern Cons a b = MkCons (a, b)
{-# COMPLETE Cons #-}

foo :: Foo String
foo = Foo "foo"

bar :: Bar String
bar = Bar "bar"

baz :: Baz String
baz = Baz "baz"

fooBar :: FooBar String
fooBar = Cons foo bar

fooBarBaz :: FooBarBaz String
fooBarBaz = Cons foo $ Cons bar baz

foo' :: Foo' String
foo' = coerceTo foo

fooBar' :: FooBar' String
fooBar' = coerceTo fooBar

fooBar2 :: FooBar String
fooBar2 = coerceFrom fooBar'

barFoo :: BarFoo String
barFoo = castValue fooBar

bazBarFoo :: BazBarFoo String
bazBarFoo = castValue fooBarBaz

fooBaz :: FooBaz String
fooBaz = castValue bazBarFoo

testConversion :: TestTree
testConversion = testCase "test conversion to multi param" $ do
  case foo' of
    Multi.Elem (Identity foo2) ->
      assertEqual "should be able to convert to canon foo"
        foo2
        "foo"

  case fooBar' of
    Multi.Cons ((Multi.Elem (Identity foo2)), (Multi.Elem (Identity bar2))) ->
      assertEqual "should be able to convert to canon foo bar"
        (foo2, bar2)
        ("foo", "bar")

  case barFoo of
    Cons (Bar bar2) (Foo foo2) ->
      assertEqual
        "foo bar should be converted to bar foo"
        (bar2, foo2)
        ("bar", "foo")

  case fooBar2 of
    Cons (Foo foo2) (Bar bar2) ->
      assertEqual
        "foo bar should be converted back to foo bar"
        (foo2, bar2)
        ("foo", "bar")

  case fooBaz of
    Cons (Foo foo2) (Baz baz2) ->
      assertEqual
        "foo bar should be converted to foo baz"
        (foo2, baz2)
        ("foo", "baz")

  case bazBarFoo of
    Cons (Baz baz2) (Cons (Bar bar2) (Foo foo2)) ->
      assertEqual
        "foo bar should be converted to bar foo"
        (baz2, (bar2, foo2))
        ("baz", ("bar", "foo"))

  withParam fooBarBaz $ do
    assertEqual "should be able to capture foo"
      (captureParam @Foo)
      (Foo "foo")

    assertEqual "should be able to capture bar"
      (captureParam @Bar)
      (Bar "bar")

    assertEqual "should be able to capture baz"
      (captureParam @Baz)
      (Baz "baz")
