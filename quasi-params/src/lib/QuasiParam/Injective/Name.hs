{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParam.Injective.Name
  ( Param
  , captureParam
  , withParam
  )
where

import GHC.Types (Symbol)

import qualified QuasiParam.Injective.Internal as Internal

type Param name a = Internal.Param Symbol name a

captureParam
  :: forall name a
   . (Param name a)
  => a
captureParam = Internal.captureParam @Symbol @name

withParam
  :: forall name a r
   . a
  -> (Param name a => r)
  -> r
withParam = Internal.withParam @Symbol @name
