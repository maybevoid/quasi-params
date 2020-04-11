{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.QuasiParam.Internal.Name
  ( Param (..)
  , withParam
  )
where

import GHC.Types (Symbol)
import Unsafe.Coerce (unsafeCoerce)

data Dict p where
  Dict :: p => Dict p

class
  Param (name :: Symbol) a | name -> a
  where
    captureParam :: a

data ParamReflector (name :: Symbol) a = ParamReflector
  { _reflectParam :: a }

withParam
  :: forall (name :: Symbol) a r
   . a
  -> ((Param name a) => r)
  -> r
withParam x cont = case dict of Dict -> cont
 where
  dict :: Dict (Param name a)
  dict = unsafeCoerce $ ParamReflector @name @a x
