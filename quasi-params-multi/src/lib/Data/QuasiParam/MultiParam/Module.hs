{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.QuasiParam.MultiParam.Module where

import Data.Kind

import Data.QuasiParam.Dict
import qualified Data.QuasiParam.Label as Label

import Data.QuasiParam.MultiParam.Sig

data Label k (label :: k)

class IsLabel label where
  type family LabelConstraint label e = (c :: Constraint) | c -> label

  captureLabel
    :: forall e
     . (LabelConstraint label e)
    => e

  withLabel
    :: forall e r
     . e
    -> (LabelConstraint label e => r)
    -> r

instance IsLabel (Label k (label ::k)) where
  type LabelConstraint (Label k label) e = Label.Param k label e

  captureLabel = Label.captureParam @k @label
  withLabel = Label.withParam @k @label

data Cell e (t :: ArgKind) = Cell (e t)

class
  ( IsLabel (ToLabel e) )
  => HasLabel (e :: ArgKind -> Type) where
    type family ToLabel e

class
  ( IsLabel l
  , LabelConstraint l e
  )
  => OnLabel l e

instance
  ( IsLabel l
  , LabelConstraint l e
  )
  => OnLabel l e

instance (HasLabel e)
  => MultiParam (Cell e) where
    type ParamConstraint (Cell e) t
      = OnLabel (ToLabel e) (Cell e t)

    withParam = withLabel @(ToLabel e)
    captureParam = captureLabel @(ToLabel e)

data Empty (t :: ArgKind) = Empty

data Elem
  k
  (label :: k)
  (e :: ArgKind -> Type)
  (t :: ArgKind)
  = Elem (e t)

data Cons
  (e1 :: ArgKind -> Type)
  (e2 :: ArgKind -> Type)
  (t :: ArgKind)
  = Cons (e1 t) (e2 t)

class NoConstraint (t :: ArgKind)
instance NoConstraint (t :: ArgKind)

class MultiParam (e :: ArgKind -> Type) where
  type family ParamConstraint
    e (t :: ArgKind)
    = (c :: Constraint)
    | c -> e t

  captureParam
    :: forall t
     . (ParamConstraint e t)
    => e t

  withParam
    :: forall t r
     . e t
    -> (ParamConstraint e t => r)
    -> r

instance MultiParam Empty where
  type ParamConstraint Empty t = NoConstraint t

  captureParam = Empty
  withParam Empty cont = cont

instance MultiParam (Elem k (label :: k) e) where
  type ParamConstraint (Elem k label e) t
    = Label.Param k label (Elem k label e t)

  captureParam = Label.captureParam @k @label
  withParam = Label.withParam @k @label

instance
  ( MultiParam e1
  , MultiParam e2
  )
  => MultiParam (Cons e1 e2) where
    type ParamConstraint (Cons e1 e2) t
      = (ParamConstraint e1 t, ParamConstraint e2 t)

    captureParam = Cons (captureParam @e1) (captureParam @e2)

    withParam (Cons e1 e2) cont =
      withParam e1 $ withParam e2 cont

class
  ( MultiParam e1
  , MultiParam e2
  )
  => EntailParam e1 e2 t where
    entailParam
      :: forall r
       . (ParamConstraint e1 t)
      => ((ParamConstraint e2 t) => r)
      -> r

instance
  ( MultiParam e1
  , MultiParam e2
  , ParamConstraint e1 t ~ p
  , ParamConstraint e2 t ~ q
  , p => q
  )
  => EntailParam e1 e2 t where
    entailParam cont = cont

class
  ( MultiParam e1
  , MultiParam e2
  )
  => CastParam e1 e2 where
    castParamDict
      :: forall t
       . (ParamConstraint e1 t)
      => Dict (ParamConstraint e2 t)

instance
  ( MultiParam e1
  , MultiParam e2
  , forall t . EntailParam e1 e2 t
  )
  => CastParam e1 e2 where
    castParamDict
      :: forall t
       . (ParamConstraint e1 t)
      => Dict (ParamConstraint e2 t)
    castParamDict = entailParam @e1 @e2 @t Dict

type family Params (xs :: [ArgKind -> Type]) :: (ArgKind -> Type) where
  Params '[] = Empty
  Params (x:'[]) = x
  Params (x:xs) = Cons x (Params xs)

entailValue
  :: forall e1 e2 t
   . (EntailParam e1 e2 t)
  => e1 t
  -> e2 t
entailValue e = withParam e $
  entailParam @e1 @e2 @t $
    captureParam @e2

castParam
  :: forall e1 e2 t r
   . ( CastParam e1 e2
     , ParamConstraint e1 t
     )
  => ((ParamConstraint e2 t) => r)
  -> r
castParam cont = case castParamDict @e1 @e2 @t of
  Dict -> cont

castValue
  :: forall e1 e2 t
   . (CastParam e1 e2)
  => e1 t
  -> e2 t
castValue e = withParam e $
  castParam @e1 @e2 @t $
    captureParam
