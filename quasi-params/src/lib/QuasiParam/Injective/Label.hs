{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParam.Injective.Label
  ( Param
  , captureParam
  , withParam
  )
where

import qualified QuasiParam.Injective.Internal as Internal

type Param k (label :: k) a = Internal.Param k label a

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
