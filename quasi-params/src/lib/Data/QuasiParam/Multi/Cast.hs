{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.QuasiParam.Multi.Cast where

import Data.Kind

import Data.QuasiParam.Dict
import Data.QuasiParam.Multi.Param
import Data.QuasiParam.Multi.Entail

class
  ( MultiParam k e1
  , MultiParam k e2
  )
  => CastParam k (e1 :: k -> Type) (e2 :: k -> Type) where
    castParamDict
      :: forall (t :: k)
       . (ParamConstraint k e1 t)
      => Dict (ParamConstraint k e2 t)

instance
  ( MultiParam k e1
  , MultiParam k e2
  , forall (t :: k) . EntailParam k e1 e2 t
  )
  => CastParam k (e1 :: k -> Type) (e2 :: k -> Type) where
    castParamDict
      :: forall (t :: k)
       . (ParamConstraint k e1 t)
      => Dict (ParamConstraint k e2 t)
    castParamDict = entailParam @k @e1 @e2 @t Dict

castParam
  :: forall k (e1 :: k -> Type) (e2 :: k -> Type) (t :: k) r
   . ( CastParam k e1 e2
     , ParamConstraint k e1 t
     )
  => ((ParamConstraint k e2 t) => r)
  -> r
castParam cont = case castParamDict @k @e1 @e2 @t of
  Dict -> cont

castValue
  :: forall k e1 e2 (t :: k)
   . (CastParam k e1 e2)
  => e1 t
  -> e2 t
castValue e = withParam e $
  castParam @k @e1 @e2 @t $
    captureParam @k @e2
