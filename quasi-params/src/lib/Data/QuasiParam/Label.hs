{-# LANGUAGE PolyKinds #-}

module Data.QuasiParam.Label
  ( Param
  , captureParam
  , withParam
  )
where

import qualified Data.QuasiParam.Internal as Internal

class (Internal.QuasiParam k label a)
  => Param k (label :: k) a

instance (Internal.QuasiParam k label a)
  => Param k (label :: k) a

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
