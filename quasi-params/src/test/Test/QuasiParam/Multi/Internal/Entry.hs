module Test.QuasiParam.Multi.Internal.Entry where

import Data.Kind
import Data.Coerce
import GHC.Types (Symbol)

import qualified Data.QuasiParam.Multi as Multi

newtype Const a (b :: ()) = Const
  { unConst :: a }

newtype Nil = Nil ()

newtype Entry (name :: Symbol) a = Entry
  { unEntry :: a }
  deriving (Eq, Show)

newtype Cons e1 e2 = MkCons
  { unCons :: (e1, e2) }

coerceConst :: Entry name a -> Const a '()
coerceConst = coerce

class
  ( Multi.MultiParam () (AsMultiParam e)
  )
  => IsMultiParam e where
    type family AsMultiParam e
      :: () -> Type

    coerceTo :: e -> AsMultiParam e '()
    coerceFrom :: AsMultiParam e '() -> e

instance IsMultiParam Nil where
  type AsMultiParam Nil = Multi.Empty ()

  coerceTo = coerce
  coerceFrom = coerce

instance IsMultiParam (Entry name a) where
  type AsMultiParam (Entry name a) = Multi.Elem Symbol () name (Const a)

  coerceTo = coerce
  coerceFrom = coerce

instance
  ( IsMultiParam e1
  , IsMultiParam e2
  , AsMultiParam e1 ~ e1'
  , AsMultiParam e2 ~ e2'
  , Multi.Cons () e1' e2' '() ~ e3
  , Coercible (Cons e1 e2) e3
  , Coercible e3 (Cons e1 e2)
  )
  => IsMultiParam (Cons e1 e2) where
    type AsMultiParam (Cons e1 e2) =
      Multi.Cons ()
        (AsMultiParam e1)
        (AsMultiParam e2)

    coerceTo = coerce
    coerceFrom = coerce

withParam
  :: forall e e' r
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam () e'
     )
  => e
  -> (Multi.ParamConstraint () e' '() => r)
  -> r
withParam e cont = Multi.withParam e' cont
 where
  e' :: e' '()
  e' = coerceTo e

captureParam
  :: forall e e'
   . ( IsMultiParam e
     , AsMultiParam e ~ e'
     , Multi.MultiParam () e'
     , Multi.ParamConstraint () e' '()
     )
  => e
captureParam = coerceFrom e'
 where
  e' :: e' '()
  e' = Multi.captureParam @() @e'

castValue
  :: forall e1 e2 e1' e2'
   . ( IsMultiParam e1
     , IsMultiParam e2
     , AsMultiParam e1 ~ e1'
     , AsMultiParam e2 ~ e2'
     , Multi.CastParam () e1' e2'
     )
  => e1
  -> e2
castValue e1 = coerceFrom e2'
 where
  e1' :: e1' '()
  e1' = coerceTo e1

  e2' :: e2' '()
  e2' = Multi.castValue e1'
