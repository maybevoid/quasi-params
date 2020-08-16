module QuasiParams.Params where

import Data.Kind (Type)
import QuasiParams.ArgKind (ArgKind)

data Nil (k :: ArgKind) = Nil

data Cons
  (e :: ArgKind -> Type)
  (p :: ArgKind -> Type)
  (k :: ArgKind)
  = Cons (e k) (p k)

data Row
  (p :: ArgKind -> Type)
  (k :: ArgKind)
  = Row (p k)

data Union'
  (p1 :: ArgKind -> Type)
  (p2 :: ArgKind -> Type)
  (k :: ArgKind)
  = Union' (p1 k) (p2 k)

type Union p1 p2 = Union' (Row p1) (Row p2)

pattern Union
  :: forall e1 e2 k
   . e1 k
  -> e2 k
  -> Union e1 e2 k
pattern Union e1 e2 = Union' (Row e1) (Row e2)
{-# COMPLETE Union #-}
