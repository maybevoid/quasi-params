{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.QuasiParam.Internal
  ( Param (..)
  , withParam
  )
where

import Unsafe.Coerce (unsafeCoerce)

import Data.QuasiParam.Dict

class
  Param k (label :: k) a | k label -> a
  where
    captureParam :: a

data ParamReflector k (label :: k) a = ParamReflector
  { _reflectParam :: a }

withParam
  :: forall k (label :: k) a r
   . a
  -> ((Param k label a) => r)
  -> r
withParam x cont = case dict of Dict -> cont
 where
  dict :: Dict (Param k label a)
  dict = unsafeCoerce $ ParamReflector @k @label @a x
