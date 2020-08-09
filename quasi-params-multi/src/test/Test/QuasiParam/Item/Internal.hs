{-# LANGUAGE PolyKinds #-}

module Test.QuasiParam.Item.Internal
  ( Item (..)
  , Items
  , pattern (:+)
  , HasLabel (..)
  , withItem
  , captureItem
  , module Test.QuasiParam.Item.Module
  )
where

import GHC.Types (Symbol)
import Test.QuasiParam.Item.Module

import QuasiParam.Name (Name)
import QuasiParam.Label (HasLabel (..))
import qualified QuasiParam.Name as Name

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

type ItemConstraint name a = Name.Param name (Item name a)

withItem
  :: forall name a r
   . Item name a
  -> (ItemConstraint name a => r)
  -> r
withItem = withSingleton

captureItem
  :: forall name a
   . (ItemConstraint name a)
  => Item name a
captureItem = captureSingleton
