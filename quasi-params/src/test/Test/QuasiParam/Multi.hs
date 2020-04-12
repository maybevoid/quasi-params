{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.QuasiParam.Multi where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Kind
import GHC.Types (Symbol)
import Data.Functor.Identity

import qualified Data.QuasiParam.Multi as Multi

tests :: TestTree
tests = testGroup "Multi parameters test"
  [ testCastFooBar
  ]

type Item tag = Multi.Elem Symbol Type tag Identity
type Union a b = Multi.Union Type (Item a) (Item b)

pattern Item :: forall tag t . t -> Item tag t
pattern Item a = Multi.Elem (Identity a)

pattern Union :: forall k1 k2 (label :: k2) (label1 :: k1) t
  . t -> t
  -> Multi.Union Type
      (Multi.Elem k2 Type label Identity)
      (Multi.Elem k1 Type label1 Identity)
      t
pattern Union a b = Multi.Union (Multi.Elem (Identity a)) (Multi.Elem (Identity b))
{-# COMPLETE Union #-}

foo :: Item "Foo" String
foo = Item "foo"

bar :: Item "Foo" String
bar = Item "bar"

fooBar :: Union "Foo" "Bar" String
fooBar = Union "foo" "bar"

barFoo :: Union "Bar" "Foo" String
barFoo = Multi.castValue fooBar

testCastFooBar :: TestTree
testCastFooBar = testCase "test cast foo bar" $
  case barFoo of
    Union bar' foo' -> do
      assertEqual "foo bar should be casted to bar foo"
        (bar', foo')
        ("bar", "foo")
