module QuasiParams.HasParam where

import Data.Proxy (Proxy (..))
import Data.Kind (Type)

import QuasiParams.Params
import QuasiParams.ArgKind (ArgKind)

class HasParam
  (tag :: Type)
  (params :: ArgKind -> Type)
  (e :: ArgKind -> Type)
  | tag params -> e
  where
    getParam
      :: forall t
       . Proxy tag
      -> params t
      -> e t

class HasLabel (e :: ArgKind -> Type) where
  type family GetLabel e :: Type

instance {-# OVERLAPPING #-}
  ( HasLabel e
  , GetLabel e ~ tag
  )
  => HasParam tag (Cons e p) e where
    getParam Proxy (Cons e _) = e

instance {-# OVERLAPPABLE #-}
  HasParam tag p e1
  => HasParam tag (Cons e2 p) e1 where
    getParam proxy (Cons _ p) = getParam proxy p

instance {-# OVERLAPPING #-}
  HasParam tag p1 e
  => HasParam tag (Union' (Row p1) p2) e where
    getParam tag (Union' (Row p) _) = getParam tag p

instance {-# OVERLAPPABLE #-}
  HasParam tag p2 e
  => HasParam tag (Union' p1 (Row p2)) e where
    getParam tag (Union' _ (Row p)) = getParam tag p
