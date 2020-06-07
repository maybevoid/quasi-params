{-# LANGUAGE PolyKinds #-}

module Test.QuasiParam.Multi.Internal.Pair where

import Data.Kind
import Data.Coerce
import GHC.Types (Symbol)

import qualified QuasiParam.Multi as Multi

newtype Const a b = Const
  { unConst :: a }

data Pair a b = Pair a b

type family First (x :: Pair Type Type) = (y :: Type) | y -> x where
  First ('Pair a b) = Const a b

type family Second (x :: Pair Type Type) = (y :: Type) | y -> x where
  Second ('Pair a b) = Const b a

newtype Payload (x :: Pair Type Type) = Payload
  { unPayload :: (First x, Second x) }

newtype Nil (a :: Type) (b :: Type) = Nil ()

newtype Item (name :: Symbol) a b = Item
  { unItem :: (a, b) }
  deriving (Eq, Show)

newtype Cons e1 e2 (a :: Type) (b :: Type) = MkCons
  { unCons :: (e1 a b, e2 a b) }
  deriving (Eq, Show)

class
  ( Multi.MultiParam (Pair Type Type) (AsMultiParam e)
  )
  => IsMultiParam (e :: Type -> Type -> Type) where
    type family AsMultiParam e
      :: Pair Type Type -> Type

    coerceTo
      :: forall a b
       . e a b
       -> AsMultiParam e ('Pair a b)

    coerceFrom
      :: forall a b
       . AsMultiParam e ('Pair a b)
      -> e a b

instance IsMultiParam Nil where
  type AsMultiParam Nil = Multi.Empty (Pair Type Type)

  coerceTo = coerce
  coerceFrom = coerce

instance IsMultiParam (Item name) where
  type AsMultiParam (Item name) = Multi.Elem Symbol (Pair Type Type) name Payload

  coerceTo = coerce
  coerceFrom = coerce

instance
  ( IsMultiParam e1
  , IsMultiParam e2
  , AsMultiParam e1 ~ e1'
  , AsMultiParam e2 ~ e2'
  , Multi.Cons (Pair Type Type) e1' e2' ~ e3
  , forall a b . Coercible (Cons e1 e2 a b) (e3 ('Pair a b))
  , forall a b . Coercible (e3 ('Pair a b)) (Cons e1 e2 a b)
  )
  => IsMultiParam (Cons e1 e2) where
    type AsMultiParam (Cons e1 e2) =
      Multi.Cons
        (Pair Type Type)
        (AsMultiParam e1)
        (AsMultiParam e2)

    coerceTo = coerce
    coerceFrom = coerce

withParam
  :: forall e e' a b r
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam (Pair Type Type) e'
     )
  => e a b
  -> (Multi.ParamConstraint (Pair Type Type) e' ('Pair a b) => r)
  -> r
withParam e cont = Multi.withParam e' cont
 where
  e' :: e' ('Pair a b)
  e' = coerceTo e

captureParam
  :: forall e e' a b
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam (Pair Type Type) e'
     , Multi.ParamConstraint (Pair Type Type) e' ('Pair a b)
     )
  => e a b
captureParam = coerceFrom e'
 where
  e' :: e' ('Pair a b)
  e' = Multi.captureParam @(Pair Type Type) @e'

castValue
  :: forall e1 e2 e1' e2' a b
   . ( IsMultiParam e1
     , IsMultiParam e2
     , AsMultiParam e1 ~ e1'
     , AsMultiParam e2 ~ e2'
     , Multi.CastParam (Pair Type Type) e1' e2'
     )
  => e1 a b
  -> e2 a b
castValue e1 = coerceFrom e2'
 where
  e1' :: e1' ('Pair a b)
  e1' = coerceTo e1

  e2' :: e2' ('Pair a b)
  e2' = Multi.castValue e1'
