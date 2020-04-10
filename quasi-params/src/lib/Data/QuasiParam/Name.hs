
module Data.QuasiParam.Name
  ( Param
  , captureParam
  , withParam
  )
where

import qualified Data.QuasiParam.Internal as Internal

import GHC.Types (Symbol)

class (Internal.QuasiParam Symbol name a)
  => Param name a

instance (Internal.QuasiParam Symbol name a)
  => Param name a

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
