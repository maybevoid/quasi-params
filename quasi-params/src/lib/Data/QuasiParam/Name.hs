{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.QuasiParam.Name
  ( Param
  , captureParam
  , withParam
  )
where

import qualified Data.QuasiParam.Internal.Name as Internal

type Param = Internal.Param

-- class (Internal.Param name a)
--   => Param name a | name -> a

-- instance (Internal.Param name a)
--   => Param name a

captureParam
  :: forall name a
   . (Param name a)
  => a
captureParam = Internal.captureParam @name

withParam
  :: forall name a r
   . a
  -> (Param name a => r)
  -> r
withParam = Internal.withParam @name
