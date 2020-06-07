{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParam.Injective.Internal
where

import Data.Kind (Constraint)
import qualified QuasiParam.Label as Label

class InjectiveParam k (label :: k) where
  type family Param k (label :: k) a
    = (c :: Constraint) | c -> k label a

  captureParam
    :: forall a
     . (Param k label a)
    => a

  withParam
    :: forall a r
     . a
    -> ((Param k label a) => r)
    -> r

instance InjectiveParam k (label :: k) where
  type Param k (label :: k) a = Label.Param k label a

  captureParam
    :: forall a
     . (Label.Param k label a)
    => a
  captureParam = Label.captureParam @k @label

  withParam
    :: forall a r
     . a
    -> ((Label.Param k (label) a) => r)
    -> r
  withParam = Label.withParam @k @label
