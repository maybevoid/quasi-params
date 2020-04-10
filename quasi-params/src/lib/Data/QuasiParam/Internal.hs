{-# LANGUAGE PolyKinds #-}

module Data.QuasiParam.Internal
  ( QuasiParam (..)
  , withParam
  )
where

import Unsafe.Coerce (unsafeCoerce)

data Dict p where
  Dict :: p => Dict p

class
  QuasiParam k (label :: k) a
  | label -> a
  where
    captureParam :: a

data ParamReflector k (label :: k) a = ParamReflector
  { _reflectParam :: a }

withParam
  :: forall k (label :: k) a r
   . a
  -> ((QuasiParam k label a) => r)
  -> r
withParam x cont = case dict of Dict -> cont
 where
  dict :: Dict (QuasiParam k label a)
  dict = unsafeCoerce $ ParamReflector @k @label @a x
