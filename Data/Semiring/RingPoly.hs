{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Semiring.RingPoly
  ( RingPoly(..)
  , collapse
  , compose
  , evaluate
  , degree
  , fromList
  , fromListN
  , toList
  , empty
  , singleton
  , scale
  , shift
  , shiftN
  ) where

import           Control.Applicative (Applicative, Alternative)
import           Control.DeepSeq (NFData)
import           Control.Monad (MonadPlus)
import           Control.Monad.Zip (MonadZip)
import           Data.Data (Data)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Data.Ord (Ordering(..), compare)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified GHC.Exts as Exts
import           GHC.Generics (Generic, Generic1)
import           Prelude hiding (Num(..))

import Data.Semiring

-- | The type of polynomials in one variable
newtype RingPoly  a   = RingPoly { unRingPoly :: Vector a }
  deriving ( Monad, Functor, Applicative, Foldable
           , Traversable, Eq1, Ord1, Read1, Show1
           , MonadZip, Alternative, MonadPlus
           , Eq, Data, Ord
           , Read, Show, Semigroup, Monoid
           , NFData
           , Generic, Generic1
           , Ring )

instance Exts.IsList (RingPoly a) where
  type Item (RingPoly a) = a
  fromList  = fromList
  fromListN = fromListN
  toList    = toList

fromList :: [a] -> RingPoly a
fromList = RingPoly . Vector.fromList

fromListN :: Int -> [a] -> RingPoly a
fromListN = (fmap RingPoly) . Vector.fromListN

toList :: RingPoly a -> [a]
toList = Vector.toList . unRingPoly

degree :: RingPoly a -> Int
degree (RingPoly p) = Vector.length p

empty :: RingPoly a
empty = RingPoly $ Vector.empty

singleton :: a -> RingPoly a
singleton = RingPoly . Vector.singleton

-- | Compose two polynomials. Illustrated:
--   compose f g = h
--   =
--   f(g(x)) = h(x)
compose :: Semiring a => RingPoly a -> RingPoly a -> RingPoly a
compose (RingPoly x) y = horner y (fmap singleton x)

-- | Compose any number of polynomials of the form
-- f0(f1(f2(...(fN(x))))) into
-- f(x)
collapse :: Semiring a => Vector (RingPoly a) -> RingPoly a
collapse = Foldable.foldr compose zero

-- | Horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

-- | Horner's scheme for evaluating a polynomial in a semiring
evaluate :: Semiring a => a -> RingPoly a -> a
evaluate x = Foldable.foldr (\c val -> c + x * val) zero

shiftN :: Semiring a => Int -> RingPoly a -> RingPoly a
shiftN d (RingPoly v) = RingPoly v'
  where
    l = Vector.length v 
    v' = Vector.generate (d + l) (\i -> if i < l then Vector.unsafeIndex v i else zero)

shift :: Semiring a => RingPoly a -> RingPoly a
shift (RingPoly xs)
  | Vector.null xs = empty
  | otherwise = RingPoly $ Vector.snoc xs zero

scale :: Semiring a => a -> RingPoly a -> RingPoly a
scale s = RingPoly . Vector.map (s *) . unRingPoly

polyPlus, polyTimes :: Semiring a => Vector a -> Vector a -> Vector a
polyPlus xs ys =
  case compare (Vector.length xs) (Vector.length ys) of
    EQ -> Vector.zipWith (+) xs ys
    LT -> Vector.unsafeAccumulate (+) ys (Vector.indexed xs)
    GT -> Vector.unsafeAccumulate (+) xs (Vector.indexed ys)
polyTimes signal kernel
  | Vector.null signal = Vector.empty
  | Vector.null kernel = Vector.empty
  | otherwise = Vector.generate (slen + klen - 1) f
  where
    f n = Foldable.foldl'
      (\a k ->
        a +
        Vector.unsafeIndex signal k *
        Vector.unsafeIndex kernel (n - k)) zero [kmin .. kmax]
      where
        !kmin = max 0 (n - (klen - 1))
        !kmax = min n (slen - 1)
    !slen = Vector.length signal
    !klen = Vector.length kernel

instance Semiring a => Semiring (RingPoly a) where
  zero = empty
  one  = singleton one

  plus  x y = RingPoly $ polyPlus  (unRingPoly x) (unRingPoly y)
  times x y = RingPoly $ polyTimes (unRingPoly x) (unRingPoly y)
