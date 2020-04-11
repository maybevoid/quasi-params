{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.QuasiParam.Injective.Tag
  ( Param
  , captureParam
  , withParam
  )
where

import qualified Data.QuasiParam.Injective.Internal as Internal

import Data.Kind (Type)

type Param tag a = Internal.Param Type tag a

captureParam
  :: forall tag a
   . (Param tag a)
  => a
captureParam = Internal.captureParam @Type @tag

withParam
  :: forall tag a r
   . a
  -> (Param tag a => r)
  -> r
withParam = Internal.withParam @Type @tag
