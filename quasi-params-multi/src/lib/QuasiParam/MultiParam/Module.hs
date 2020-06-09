{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module QuasiParam.MultiParam.Module where

import Data.Kind

import QuasiParam.Dict
import QuasiParam.Label
  ( IsLabel (..)
  )

import QuasiParam.MultiParam.Sig

data Nil (t :: ArgKind) = Nil

data Singleton e (t :: ArgKind)
  = Singleton { unSingleton :: e t }

data Union
  (e1 :: ArgKind -> Type)
  (e2 :: ArgKind -> Type)
  (t :: ArgKind)
  = Union (e1 t) (e2 t)

data Cons
  (e1 :: ArgKind -> Type)
  (e2 :: ArgKind -> Type)
  (t :: ArgKind)
  = Cons (e1 t) (e2 t)

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

class NoConstraint (t :: ArgKind)
instance NoConstraint t

instance MultiParam Nil where
  type ParamConstraint Nil t = NoConstraint t

  captureParam = Nil
  withParam Nil cont = cont

class (c1, c2) => UnionConstraint c1 c2
instance (c1, c2) => UnionConstraint c1 c2

instance
  ( MultiParam e1
  , MultiParam e2
  )
  => MultiParam (Union e1 e2) where
    type ParamConstraint (Union e1 e2) t
      = UnionConstraint
          (ParamConstraint e1 t)
          (ParamConstraint e2 t)

    captureParam = Union (captureParam @e1) (captureParam @e2)

    withParam (Union e1 e2) cont =
      withParam e1 $ withParam e2 cont

class
  ( IsLabel (GetLabel e) )
  => HasLabel (e :: ArgKind -> Type) where
    type family GetLabel e

class (LabelConstraint l e) => SingletonConstraint l e
instance (LabelConstraint l e) => SingletonConstraint l e

instance (HasLabel e, IsLabel (GetLabel e))
  => MultiParam (Singleton e) where
    type ParamConstraint (Singleton e) t
      = SingletonConstraint (GetLabel e) (Singleton e t)

    withParam = withLabel @(GetLabel e)
    captureParam = captureLabel @(GetLabel e)

class ( LabelConstraint label e, c ) => ConsConstraint label e c
instance ( LabelConstraint label e, c ) => ConsConstraint label e c

instance
  ( HasLabel e1
  , IsLabel (GetLabel e1)
  , MultiParam e2
  )
  => MultiParam (Cons e1 e2) where
    type ParamConstraint (Cons e1 e2) t
      = ConsConstraint
          (GetLabel e1)
          (e1 t)
          (ParamConstraint e2 t)

    withParam (Cons e1 e2) cont
      = withLabel @(GetLabel e1) e1 $ withParam e2 cont

    captureParam = Cons (captureLabel @(GetLabel e1)) (captureParam @e2)

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

type EntailDict e1 e2 t
  = (ParamConstraint e1 t) => Dict (ParamConstraint e2 t)

type CastDict e1 e2 = forall t . EntailDict e1 e2 t

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
    castDict :: CastDict e1 e2

instance
  ( MultiParam e1
  , MultiParam e2
  , forall t . EntailParam e1 e2 t
  )
  => CastParam e1 e2 where
    castDict
      :: forall t
       . (ParamConstraint e1 t)
      => Dict (ParamConstraint e2 t)
    castDict = entailParam @e1 @e2 @t Dict

type family Params (xs :: [ArgKind -> Type]) :: (ArgKind -> Type) where
  Params '[] = Nil
  Params (x:xs) = Cons x (Params xs)

withSingleton
  :: forall e t r
   . (HasLabel e)
  => e t
  -> (ParamConstraint (Singleton e) t => r)
  -> r
withSingleton e = withParam (Singleton e)

captureSingleton
  :: forall e t
   . (HasLabel e, ParamConstraint (Singleton e) t)
  => e t
captureSingleton = unSingleton captureParam

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
castParam cont = case castDict @e1 @e2 @t of
  Dict -> cont

castValue
  :: forall e1 e2 t
   . (CastParam e1 e2)
  => e1 t
  -> e2 t
castValue e = withParam e $
  castParam @e1 @e2 @t $
    captureParam

castValueWithDict
  :: forall e1 e2 t
   . ( MultiParam e1
     , MultiParam e2
     )
  => CastDict e1 e2
  -> e1 t
  -> e2 t
castValueWithDict dict e = withParam e $
  case dict @t of
    Dict -> captureParam

entailDict
  :: forall e1 e2 t
   . (EntailParam e1 e2 t)
  => EntailDict e1 e2 t
entailDict = entailParam @e1 @e2 @t Dict

extendCast
  :: forall e1 e2 e3
   . CastDict e1 e2
  -> CastDict (Union e1 e3) (Union e2 e3)
extendCast cast1 = cast2
 where
  cast2
    :: forall t
     . EntailDict (Union e1 e3) (Union e2 e3) t
  cast2 = case cast1 @t of Dict -> Dict

composeCast
  :: forall e1 e2 e3
   . CastDict e1 e2
  -> CastDict e2 e3
  -> CastDict e1 e3
composeCast cast1 cast2 = cast3
 where
  cast3
    :: forall t
     . EntailDict e1 e3 t
  cast3 = case (cast1 @t) of
    Dict -> case (cast2 @t) of
      Dict -> Dict
