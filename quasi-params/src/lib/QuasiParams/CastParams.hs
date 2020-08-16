{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module QuasiParams.CastParams where

import QuasiParams.MultiParam

data Dict c = c => Dict

type EntailDict p1 p2 p3 =
  (ParamConstraint p1 p3) => Dict (ParamConstraint p2 p3)

type CastDict p1 p2 = forall p3 . EntailDict p1 p2 p3

class
  ( MultiParam p1
  , MultiParam p2
  )
  => EntailParam p1 p2 p3 where
    entailParam
      :: forall r
       . ((ParamConstraint p2 p3) => r)
      -> ((ParamConstraint p1 p3) => r)

instance
  ( MultiParam p1
  , MultiParam p2
  , ParamConstraint p1 p3 ~ c1
  , ParamConstraint p2 p3 ~ c2
  , c1 => c2
  )
  => EntailParam p1 p2 p3 where
    entailParam cont = cont

class
  ( MultiParam p1
  , MultiParam p2
  )
  => CastParam p1 p2 where
    castDict :: CastDict p1 p2

instance
  ( MultiParam p1
  , MultiParam p2
  , forall p3 . EntailParam p1 p2 p3
  )
  => CastParam p1 p2 where
    castDict
      :: forall p3
      . EntailDict p1 p2 p3
    castDict = entailParam @p1 @p2 @p3 Dict
