{-# LANGUAGE PolyKinds #-}

module Test.QuasiParam.Item.Internal
  ( LabeledItem
  , Nil
  , Union
  , Item
  , Items
  , pattern (:+)
  , pattern Nil
  , pattern Item
  , pattern LabeledItem

  , Multi.MultiParam (..)
  , Multi.EntailParam (..)
  , Multi.CastParam (..)

  , Multi.entailValue
  , Multi.castParam
  , Multi.castValue
  )

where

import Data.Kind
import Data.Functor.Identity

import GHC.Types (Symbol)
import Data.QuasiParam.Item as Multi

type LabeledItem k (label :: k)
  = Multi.Elem k label Identity

type Item name = LabeledItem Symbol name

type Nil = Multi.Empty

type Union = Multi.Cons

pattern Nil :: forall t . Nil t
pattern Nil = Multi.Empty
{-# COMPLETE Nil #-}

pattern LabeledItem
  :: forall k (label :: k) t
   . t
  -> LabeledItem k label t
pattern LabeledItem e = Multi.Elem (Identity e)

pattern Item
  :: forall name t
   . t
  -> Item name t
pattern Item e = LabeledItem e
{-# COMPLETE Item #-}

infixr 7 :+
pattern (:+)
  :: forall e1 e2 t
   . e1 t
  -> e2 t
  -> Union e1 e2 t
pattern e1 :+ e2 = Multi.Cons e1 e2
{-# COMPLETE (:+) #-}

type family Items (xs :: [Type -> Type]) :: (Type -> Type) where
  Items '[] = Nil
  Items (x:xs) = Union x (Items xs)
