{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.QuasiParam
  ( LabelledParam
  , TaggedParam
  , NamedParam
  , captureLabel
  , withLabel
  , captureTag
  , withTag
  , captureName
  , withName
  )
where

import qualified Data.QuasiParam.Label as Label
import qualified Data.QuasiParam.Tag as Tag
import qualified Data.QuasiParam.Name as Name

type LabelledParam k (label :: k) = Label.Param k label
type TaggedParam = Tag.Param
type NamedParam name a = Name.Param name a

captureLabel
  :: forall k (label :: k) a
   . (LabelledParam k label a)
  => a
captureLabel = Label.captureParam @k @label

withLabel
  :: forall k (label :: k) a r
   . a
  -> (LabelledParam k label a => r)
  -> r
withLabel = Label.withParam @k @label

captureTag
  :: forall tag a
   . (TaggedParam tag a)
  => a
captureTag = Tag.captureParam @tag

withTag
  :: forall tag a r
   . a
  -> (TaggedParam tag a => r)
  -> r
withTag = Tag.withParam @tag

captureName
  :: forall name a
   . (NamedParam name a)
  => a
captureName = Name.captureParam @name

withName
  :: forall name a r
   . a
  -> (NamedParam name a => r)
  -> r
withName = Name.withParam @name
