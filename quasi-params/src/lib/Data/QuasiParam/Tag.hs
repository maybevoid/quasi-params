{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.QuasiParam.Tag
  ( Param
  , captureParam
  , withParam
  )
where

import qualified Data.QuasiParam.Internal.Label as Internal

import Data.Kind (Type)

class (Internal.Param Type tag a)
  => Param tag a | tag -> a

instance (Internal.Param Type tag a)
  => Param tag a

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
