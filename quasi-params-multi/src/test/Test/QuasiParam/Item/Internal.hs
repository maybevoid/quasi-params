{-# LANGUAGE PolyKinds #-}

module Test.QuasiParam.Item.Internal
  ( Item (..)
  , Items
  , pattern (:+)
  , module Data.QuasiParam.Item
  )
where

import GHC.Types (Symbol)
import Data.QuasiParam.Item

import Data.QuasiParam.Name (Name)

data Item (name :: Symbol) a = Item a

instance HasLabel (Item name) where
  type GetLabel (Item name) = Name name

type Items xs = Params xs

infixr 7 :+
pattern (:+)
  :: forall e1 e2 t
   . e1 t
  -> e2 t
  -> Cons e1 e2 t
pattern e1 :+ e2 = Cons e1 e2
{-# COMPLETE (:+) #-}
