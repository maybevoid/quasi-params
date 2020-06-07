{-# LANGUAGE PolyKinds #-}

module Test.QuasiParam.Multi.Internal.Bifunctor where

import Data.Kind
import Data.Coerce
import GHC.Types (Symbol)

import qualified QuasiParam.Multi as Multi

data Proxy (a :: k) = Proxy

newtype Const (f :: Type -> Type) (g :: Type -> Type) (a :: Type)  = Const
  { unConst :: f a }

data Pair a b = Pair a b

type FunctorPair = Pair (Type -> Type) (Type -> Type)

type family First (x :: FunctorPair) = (y :: Type -> Type) | y -> x where
  First ('Pair f g) = Const f g

type family Second (x :: FunctorPair) = (y :: Type -> Type) | y -> x where
  Second ('Pair f g) = Const g f

newtype Payload (a :: Type) (x :: FunctorPair) = Payload
  { unPayload :: (First x a, Second x a) }

newtype Nil (f :: Type -> Type) (g :: Type -> Type) = Nil ()

newtype Item (name :: Symbol) a (f :: Type -> Type) (g :: Type -> Type) = Item
  { unItem :: (f a, g a) }
  deriving (Eq, Show)

newtype Cons e1 e2 (f :: Type -> Type) (g :: Type -> Type) = MkCons
  { unCons :: (e1 f g, e2 f g) }
  deriving (Eq, Show)

castCons
  :: forall name1 name2 a b f g
   . Cons (Item name1 a) (Item name2 b) f g
  -> Multi.Cons FunctorPair
      (Multi.Elem Symbol FunctorPair name1 (Payload a))
      (Multi.Elem Symbol FunctorPair name2 (Payload b))
      ('Pair f g)
castCons = coerce

class
  ( Multi.MultiParam FunctorPair (AsMultiParam e)
  )
  => IsMultiParam (e :: (Type -> Type) -> (Type -> Type) -> Type) where
    type family AsMultiParam e
      = (e2 :: FunctorPair -> Type) | e2 -> e

    coerceTo
      :: forall f g
       . e f g
       -> AsMultiParam e ('Pair f g)

    coerceFrom
      :: forall f g
       . AsMultiParam e ('Pair f g)
      -> e f g

instance IsMultiParam Nil where
  type AsMultiParam Nil = Multi.Empty FunctorPair

  coerceTo = coerce
  coerceFrom = coerce

instance IsMultiParam (Item name a) where
  type AsMultiParam (Item name a) =
    Multi.Elem Symbol FunctorPair name (Payload a)

  coerceTo = coerce
  coerceFrom = coerce

instance
  ( IsMultiParam e1
  , IsMultiParam e2
  , AsMultiParam e1 ~ e1'
  , AsMultiParam e2 ~ e2'
  , Multi.Cons FunctorPair e1' e2' ~ e3
  , forall f g . Coercible (Cons e1 e2 f g) (e3 ('Pair f g))
  , forall f g . Coercible (e3 ('Pair f g)) (Cons e1 e2 f g)
  )
  => IsMultiParam (Cons e1 e2) where
    type AsMultiParam (Cons e1 e2) =
      Multi.Cons
        FunctorPair
        (AsMultiParam e1)
        (AsMultiParam e2)

    coerceTo = coerce
    coerceFrom = coerce

withParam
  :: forall e e' f g r
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam FunctorPair e'
     )
  => e f g
  -> (Multi.ParamConstraint FunctorPair e' ('Pair f g) => r)
  -> r
withParam e cont = Multi.withParam e' cont
 where
  e' :: e' ('Pair f g)
  e' = coerceTo e

captureParam
  :: forall e e' f g
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam FunctorPair e'
     , Multi.ParamConstraint FunctorPair e' ('Pair f g)
     )
  => e f g
captureParam = coerceFrom e'
 where
  e' :: e' ('Pair f g)
  e' = Multi.captureParam @FunctorPair @e'

castValue
  :: forall e1 e2 e1' e2' f g
   . ( IsMultiParam e1
     , IsMultiParam e2
     , AsMultiParam e1 ~ e1'
     , AsMultiParam e2 ~ e2'
     , Multi.CastParam FunctorPair e1' e2'
     )
  => e1 f g
  -> e2 f g
castValue e1 = coerceFrom e2'
 where
  e1' :: e1' ('Pair f g)
  e1' = coerceTo e1

  e2' :: e2' ('Pair f g)
  e2' = Multi.castValue e1'
