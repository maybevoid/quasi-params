{-# LANGUAGE UndecidableSuperClasses #-}

module QuasiParams.MultiParam where

import Data.Kind (Type, Constraint)
import QuasiParams.ArgKind (ArgKind)
import QuasiParams.Params
import QuasiParams.HasParam

class MultiParam (p1 :: ArgKind -> Type) where
  type family ParamConstraint
    p1
    (p2 :: ArgKind -> Type)
    = (c :: Constraint)
    | c -> p1 p2

type family Params (xs :: [ArgKind -> Type])
  = (ys :: ArgKind -> Type) | ys -> xs where
    Params ('[]) = Nil
    Params (x ': xs) = Cons x (Params xs)

class NoConstraint
  (p :: ArgKind -> Type)

instance NoConstraint
  (p :: ArgKind -> Type)

instance MultiParam Nil where
  type ParamConstraint
    Nil p = NoConstraint p

class
  ( HasLabel e
  , GetLabel e ~ label
  , HasParam label p e
  )
  => LabelConstraint label p e where

instance {-# INCOHERENT #-}
  ( HasLabel e
  , GetLabel e ~ label
  , HasParam label p e
  )
  => LabelConstraint label p e where

instance {-# INCOHERENT #-}
  ( MultiParam p1
  , HasLabel e
  )
  => MultiParam (Cons e p1) where
    type ParamConstraint (Cons e p1) p2 =
      (LabelConstraint (GetLabel e) p2 e, ParamConstraint p1 p2)

class
  ( MultiParam p1
  , MultiParam p2
  , ParamConstraint p1 p3
  , ParamConstraint p2 p3
  )
  => UnionConstraint p1 p2 p3

instance
  ( MultiParam p1
  , MultiParam p2
  , ParamConstraint p1 p3
  , ParamConstraint p2 p3
  )
  => UnionConstraint p1 p2 p3

instance
  ( MultiParam p1
  , MultiParam p2
  )
  => MultiParam (Union p1 p2) where
    type ParamConstraint (Union p1 p2) p3 =
      UnionConstraint p1 p2 p3
