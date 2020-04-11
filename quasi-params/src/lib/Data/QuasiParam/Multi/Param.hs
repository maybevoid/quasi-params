{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.QuasiParam.Multi.Param where

import qualified Data.QuasiParam.Label as Label

import Data.Kind (Type, Constraint)

data Elem k1 k2 (label :: k1) (e :: k2 -> Type) (t :: k2) where
  Elem :: e t -> Elem k1 k2 label e t

data Union k (e1 :: k -> Type) (e2 :: k -> Type) (t :: k) where
  Union :: e1 t -> e2 t -> Union k e1 e2 t

class MultiParam k (e :: k -> Type) where
  type family ParamConstraint k (e :: k -> Type) (t :: k)
    = (c :: Constraint) | c -> k e t

  captureParam
    :: forall (t :: k)
     . (ParamConstraint k e t)
    => e t

  withParam
    :: forall (t :: k) r
     . e t
    -> (ParamConstraint k e t => r)
    -> r

instance MultiParam k2 (Elem k1 k2 (label :: k1) (e :: k2 -> Type)) where
  type ParamConstraint k2 (Elem k1 k2 (label :: k1) (e :: k2 -> Type)) (t :: k2)
    = Label.Param k1 label (e t)

  captureParam
    :: forall (t :: k2)
     . (Label.Param k1 label (e t))
    => Elem k1 k2 label e t
  captureParam = Elem $ Label.captureParam @k1 @label

  withParam
    :: forall (t :: k2) r
     . Elem k1 k2 label e t
    -> ((Label.Param k1 label (e t)) => r)
    -> r
  withParam (Elem e) = Label.withParam @k1 @label e

instance
  ( MultiParam k e1
  , MultiParam k e2
  )
  => MultiParam k (Union k e1 e2) where
    type ParamConstraint k (Union k e1 e2) (t :: k)
      = (ParamConstraint k e1 t, ParamConstraint k e2 t)

    captureParam
      :: forall (t :: k)
       . (ParamConstraint k e1 t, ParamConstraint k e2 t)
      => Union k e1 e2 t
    captureParam = Union (captureParam @k @e1) (captureParam @k @e2)

    withParam
      :: forall (t :: k) r
       . Union k e1 e2 t
      -> ((ParamConstraint k e1 t, ParamConstraint k e2 t) => r)
      -> r
    withParam (Union e1 e2) cont =
      withParam e1 $ withParam e2 $ cont
