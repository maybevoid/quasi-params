{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParam.Tag
  ( Param
  , Tag
  , IsTag
  , captureParam
  , withParam
  )
where

import qualified QuasiParam.Label as Label

import Data.Kind (Type)

type Param tag = Label.Param Type tag

type Tag = Label.Label Type

class (Label.IsLabel tag) => IsTag tag

instance IsTag (Label.Label Type name)


captureParam
  :: forall tag a
   . (Param tag a)
  => a
captureParam = Label.captureParam @Type @tag

withParam
  :: forall tag a r
   . a
  -> (Param tag a => r)
  -> r
withParam = Label.withParam @Type @tag
