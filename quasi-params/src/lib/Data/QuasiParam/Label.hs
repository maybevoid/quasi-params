{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.QuasiParam.Label
  ( Param
  , Label
  , IsLabel (..)
  , captureParam
  , withParam
  )
where

import Data.Kind

import qualified Data.QuasiParam.Internal as Internal

type Param k (label :: k) = Internal.Param k label

data Label k (label :: k)

class IsLabel label where
  type family LabelConstraint label e = (c :: Constraint) | c -> label

  captureLabel
    :: forall e
     . (LabelConstraint label e)
    => e

  withLabel
    :: forall e r
     . e
    -> (LabelConstraint label e => r)
    -> r

instance IsLabel (Label k (label ::k)) where
  type LabelConstraint (Label k label) e = Param k label e

  captureLabel = captureParam @k @label
  withLabel = withParam @k @label

captureParam
  :: forall k (label :: k) a
   . (Param k label a)
  => a
captureParam = Internal.captureParam @k @label

withParam
  :: forall k (label :: k) a r
   . a
  -> (Param k label a => r)
  -> r
withParam = Internal.withParam @k @label
