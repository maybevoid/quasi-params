{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.QuasiParam.Multi.Entail where

import Data.QuasiParam.Multi.Param

class
  ( MultiParam k e1
  , MultiParam k e2
  )
  => EntailParam k e1 e2 (t :: k) where
    entailParam
      :: forall r
       . (ParamConstraint k e1 t)
      => ((ParamConstraint k e2 t) => r)
      -> r

instance
  ( MultiParam k e1
  , MultiParam k e2
  , p => q
  , p ~ ParamConstraint k e1 t
  , q ~ ParamConstraint k e2 t
  )
  => EntailParam k e1 e2 (t :: k) where
    entailParam cont = cont

entailValue
  :: forall k e1 e2 (t :: k)
   . (EntailParam k e1 e2 t)
  => e1 t
  -> e2 t
entailValue e = withParam e $
  entailParam @k @e1 @e2 @t $
    captureParam @k @e2
