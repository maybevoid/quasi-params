module Test.QuasiParam.Multi.Internal.Item where

import Data.Kind
import Data.Coerce
import GHC.Types (Symbol)
import Data.Functor.Identity

import qualified Data.QuasiParam.Multi as Multi

newtype Nil a = Nil ()

newtype Item (name :: Symbol) a = Item
  { unItem :: a }
  deriving (Eq, Show)

newtype Cons e1 e2 a = MkCons
  { unCons :: (e1 a, e2 a) }
  deriving (Eq)

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
  , Multi.Cons Type e1' e2' ~ e3
  , forall a. Coercible (Cons e1 e2 a) (e3 a)
  , forall a. Coercible (e3 a) (Cons e1 e2 a)
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
     )
  => e a
  -> (Multi.ParamConstraint Type e' a => r)
  -> r
withParam e cont = Multi.withParam e' cont
 where
  e' :: e' a
  e' = coerceTo e

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
