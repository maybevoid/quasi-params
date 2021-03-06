{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParam.Name
  ( Param
  , Name
  , IsName
  , captureParam
  , withParam
  )
where

import GHC.Types (Symbol)

import qualified QuasiParam.Label as Label

class (Label.Param Symbol name a) => Param name a
instance (Label.Param Symbol name a) => Param name a

type Name = Label.Label Symbol

class (Label.IsLabel name) => IsName name

instance IsName (Label.Label Symbol name)

captureParam
  :: forall name a
   . (Param name a)
  => a
captureParam = Label.captureParam @Symbol @name

withParam
  :: forall name a r
   . a
  -> (Param name a => r)
  -> r
withParam = Label.withParam @Symbol @name
