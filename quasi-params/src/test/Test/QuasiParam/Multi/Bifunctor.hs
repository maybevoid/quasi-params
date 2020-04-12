module Test.QuasiParam.Multi.Bifunctor where

import Test.Tasty
import Test.Tasty.HUnit
import GHC.Types (Symbol)
import Data.Functor.Identity

import qualified Data.QuasiParam.Multi as Multi
import Test.QuasiParam.Multi.Internal.Bifunctor

tests :: TestTree
tests = testGroup "Bifunctor multi parameters tests"
  [ testConversion
  ]

type Foo = Item "Foo" String
type Bar = Item "Bar" String
type Baz = Item "Baz" String

type FooBar = Cons Foo Bar
type BarFoo = Cons Bar Foo
type FooBaz = Cons Foo Baz
type FooBarBaz = Cons Foo (Cons Bar Baz)
type BazBarFoo = Cons Baz (Cons Bar Foo)

type Entry (name :: Symbol) = Item name String Identity Identity

pattern Cons :: forall e1 e2 f g . e1 f g -> e2 f g -> Cons e1 e2 f g
pattern Cons a b = MkCons (a, b)
{-# COMPLETE Cons #-}

pattern Entry :: forall name . String -> String -> Entry name
pattern Entry a b = Item (Identity a, Identity b)
{-# COMPLETE Entry #-}

pattern Foo :: String -> String -> Entry "Foo"
pattern Foo a b = Entry a b
{-# COMPLETE Foo #-}

pattern Bar :: String -> String -> Entry "Bar"
pattern Bar a b = Entry a b
{-# COMPLETE Bar #-}

pattern Baz :: String -> String -> Entry "Baz"
pattern Baz a b = Entry a b
{-# COMPLETE Baz #-}

foo :: Entry "Foo"
foo = Foo "foo1" "foo2"

bar :: Entry "Bar"
bar = Bar "bar1" "bar2"

baz :: Entry "Baz"
baz = Baz "baz1" "baz2"

fooBar :: FooBar Identity Identity
fooBar = Cons foo bar

fooBar2 :: FooBar Identity Identity
fooBar2 = castValue fooBar

barFoo :: BarFoo Identity Identity
barFoo = castValue fooBar

fooBarBaz :: FooBarBaz Identity Identity
fooBarBaz = Cons foo $ Cons bar baz

bazBarFoo :: BazBarFoo Identity Identity
bazBarFoo = castValue fooBarBaz

testConversion :: TestTree
testConversion = testCase "test conversion to multi param bifunctor" $ do
  case bazBarFoo of
    Cons (Baz baz1 baz2) (Cons (Bar bar1 bar2) (Foo foo1 foo2)) ->
      assertEqual "should be able to convert to baz bar foo"
        (foo1, foo2, bar1, bar2, baz1, baz2)
        ("foo1", "foo2", "bar1", "bar2", "baz1", "baz2")
