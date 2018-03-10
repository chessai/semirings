{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Tropical.Min 
  ( Tropical(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Semiring (Semiring(..))
import GHC.Generics (Generic)

data Tropical a = Tropical a | Infinity

deriving instance Eq a => Eq (Tropical a)
deriving instance Ord a => Ord (Tropical a)
deriving instance Show a => Show (Tropical a)
deriving instance Generic (Tropical a)

tropical :: b -> (a -> b) -> Tropical a -> b
tropical n _ Infinity = n
tropical _ f (Tropical x) = f x

instance (Ord a, Semiring a) => Semiring (Tropical a) where
  zero = Infinity
  
  Infinity `plus` y = y
  x `plus` Infinity = x
  (Tropical a) `plus` (Tropical b) = Tropical (min a b)
  
  Infinity `times` _ = Infinity
  _ `times` Infinity = Infinity
  times (Tropical a) (Tropical b) = Tropical (times a b)
  
  one  = Tropical zero

instance Functor Tropical where
  fmap _ Infinity = Infinity
  fmap f (Tropical a) = Tropical (f a)

instance Applicative Tropical where
  pure = Tropical
  _ <*> Infinity = Infinity
  Infinity <*> _ = Infinity
  (Tropical f) <*> (Tropical a) = Tropical (f a)

instance Monad Tropical where
  return = pure
  (Tropical a) >>= f = f a
  Infinity     >>= _ = Infinity
  
  (>>) = (*>)
  
  fail _             = Infinity

instance Alternative Tropical where
  empty = Infinity
  Infinity <|> r = r
  l        <|> _ = l

instance MonadPlus Tropical where

instance Foldable Tropical where
  foldMap = tropical mempty

  foldr _ z Infinity = z
  foldr f z (Tropical x) = f x z

  foldl _ z Infinity = z
  foldl f z (Tropical x) = f z x

instance Traversable Tropical where
  traverse _ Infinity = pure Infinity
  traverse f (Tropical x) = Tropical <$> f x

instance Semigroup a => Semigroup (Tropical a) where
  Infinity <> y = y
  x <> Infinity = x
  (Tropical a) <> (Tropical b) = Tropical (a <> b)

instance Monoid a => Monoid (Tropical a) where
  mempty = Infinity
  x `mappend` Infinity = x
  Infinity `mappend` y = y
  (Tropical a) `mappend` (Tropical b) = Tropical (a `mappend` b)


