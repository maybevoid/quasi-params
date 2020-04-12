{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.QuasiParam.Multi where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Kind
import Data.Coerce
import GHC.Types (Symbol)
import Data.Functor.Identity

import qualified Data.QuasiParam.Multi as Multi

tests :: TestTree
tests = testGroup "Multi parameters test"
  [ testConversion
  ]

newtype Nil a = Nil ()

newtype Item (name :: Symbol) a = Item
  { unItem :: a }

newtype Cons e1 e2 a = MkCons
  { unCons :: (e1 a, e2 a) }

class
  ( Multi.MultiParam Type (AsMultiParam e)
  )
  => IsMultiParam (e :: Type -> Type) where
    type family AsMultiParam e
      = (e2 :: Type -> Type) | e2 -> e

    coerceTo :: forall a . e a -> AsMultiParam e a
    coerceFrom :: forall a . AsMultiParam e a -> e a

instance IsMultiParam Nil where
  type AsMultiParam Nil = Multi.Empty Type

  coerceTo = coerce
  coerceFrom = coerce

instance IsMultiParam (Item name) where
  type AsMultiParam (Item name) = Multi.Elem Symbol Type name Identity

  coerceTo = coerce
  coerceFrom = coerce

instance
  ( IsMultiParam e1
  , IsMultiParam e2
  , AsMultiParam e1 ~ e1'
  , AsMultiParam e2 ~ e2'
  , forall a. Coercible (e1 a) (e1' a)
  , forall a. Coercible (e1' a) (e1 a)
  , forall a. Coercible (e2 a) (e2' a)
  , forall a. Coercible (e2' a) (e2 a)
  )
  => IsMultiParam (Cons e1 e2) where
    type AsMultiParam (Cons e1 e2) =
      Multi.Cons Type
        (AsMultiParam e1)
        (AsMultiParam e2)

    coerceTo = coerce
    coerceFrom = coerce

withParam
  :: forall e e' a r
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam Type e'
     , Coercible e e'
     )
  => e a
  -> (Multi.ParamConstraint Type e' a => r)
  -> r
withParam e cont = Multi.withParam e' cont
 where
  e' :: e' a
  e' = coerce e

captureParam
  :: forall e e' a
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam Type e'
     , Multi.ParamConstraint Type e' a
     )
  => e a
captureParam = coerceFrom e'
 where
  e' :: e' a
  e' = Multi.captureParam @Type @e'

castValue
  :: forall e1 e2 e1' e2' a
   . ( IsMultiParam e1
     , IsMultiParam e2
     , AsMultiParam e1 ~ e1'
     , AsMultiParam e2 ~ e2'
     , Multi.CastParam Type e1' e2'
     )
  => e1 a
  -> e2 a
castValue e1 = coerceFrom e2'
 where
  e1' :: e1' a
  e1' = coerceTo e1

  e2' :: e2' a
  e2' = Multi.castValue e1'

type Foo = Item "Foo"
type Bar = Item "Bar"
type FooBar = Cons Foo Bar
type BarFoo = Cons Bar Foo

type Foo' = Multi.Elem Symbol Type "Foo" Identity
type Bar' = Multi.Elem Symbol Type "Bar" Identity
type FooBar' = Multi.Cons Type Foo' Bar'
type BarFoo' = Multi.Cons Type Bar' Foo'

pattern Foo :: forall a . a -> Item "Foo" a
pattern Foo a = Item a
{-# COMPLETE Foo #-}

pattern Bar :: forall a . a -> Item "Bar" a
pattern Bar a = Item a
{-# COMPLETE Bar #-}

pattern Cons :: forall e1 e2 a . e1 a -> e2 a -> Cons e1 e2 a
pattern Cons a b = MkCons (a, b)
{-# COMPLETE Cons #-}

foo :: Foo String
foo = Foo "foo"

bar :: Bar String
bar = Bar "bar"

fooBar :: FooBar String
fooBar = Cons foo bar

foo' :: Foo' String
foo' = coerceTo foo

fooBar' :: FooBar' String
fooBar' = coerceTo fooBar

fooBar2 :: FooBar String
fooBar2 = coerceFrom fooBar'

barFoo :: BarFoo String
barFoo = castValue fooBar

testConversion :: TestTree
testConversion = testCase "test conversion to multi param" $ do
  case barFoo of
    Cons (Bar bar2) (Foo foo2) ->
      assertEqual
        "foo bar should be converted to bar foo"
        (bar2, foo2)
        ("bar", "foo")
