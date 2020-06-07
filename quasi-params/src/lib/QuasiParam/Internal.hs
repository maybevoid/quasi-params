{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParam.Internal
  ( Param (..)
  , withParam
  )
where

import Data.Kind
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import QuasiParam.Dict

class
  Param k (label :: k) a
  where
    captureParam :: a

instance
  TypeError
    ( 'Text "Named parameter cannot be found: " ':<>: 'ShowType name
      ':<>: 'Text ". The constraint (QuasiParam.Name.Param "
      ':<>: 'ShowType name ':<>: 'Text " "
      ':<>: 'ShowType a
      ':<>: 'Text ") is not satisfied"
    )
  => Param Symbol name a where
    captureParam = error "unreachable"

instance
  TypeError
    ( 'Text "Tagged parameter cannot be found: " ':<>: 'ShowType tag
      ':<>: 'Text ". The constraint (QuasiParam.Name.Param "
      ':<>: 'ShowType tag ':<>: 'Text " "
      ':<>: 'ShowType a
      ':<>: 'Text ") is not satisfied"
    )
  => Param Type tag a where
    captureParam = error "unreachable"

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
