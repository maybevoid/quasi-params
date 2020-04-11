{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.QuasiParam.Multi
  ( Internal.Elem (..)
  , Internal.Union (..)
  , MultiParam
  , ParamConstraint
  , CastParam
  , EntailParam
  , captureParam
  , withParam
  , entailParam
  , Internal.castParam
  , Internal.entailValue
  , Internal.castValue
  )
where

import Data.Kind

import qualified Data.QuasiParam.Multi.Param as Internal
import qualified Data.QuasiParam.Multi.Entail as Internal
import qualified Data.QuasiParam.Multi.Cast as Internal

type MultiParam k (e :: k -> Type)
  = Internal.MultiParam k e

type ParamConstraint k (e :: k -> Type) (t :: k)
  = Internal.ParamConstraint k e t

type EntailParam k e1 e2 (t :: k)
  = Internal.EntailParam k e1 e2 t

type CastParam k (e1 :: k -> Type) (e2 :: k -> Type)
  = Internal.CastParam k e1 e2

captureParam
  :: forall k (e :: k -> Type) (t :: k)
    . ( MultiParam k e
      , ParamConstraint k e t
      )
  => e t
captureParam = Internal.captureParam @k @e

withParam
  :: forall k (e :: k -> Type) (t :: k) r
   . (MultiParam k e)
  => e t
  -> (ParamConstraint k e t => r)
  -> r
withParam = Internal.withParam @k @e @t

entailParam
  :: forall k e1 e2 (t :: k) r
   . ( EntailParam k e1 e2 t
     , ParamConstraint k e1 t
     )
  => ((ParamConstraint k e2 t) => r)
  -> r
entailParam = Internal.entailParam @k @e1 @e2 @t
