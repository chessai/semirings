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

module Data.Semiring.Poly
  ( Poly(..)
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
  , unShift
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
newtype Poly  a   = Poly { unPoly :: Vector a }
  deriving ( Monad, Functor, Applicative, Foldable
           , Traversable, Eq1, Ord1, Read1, Show1
           , MonadZip, Alternative, MonadPlus
           , Eq, Data, Ord
           , Read, Show, Semigroup, Monoid
           , NFData
           , Generic, Generic1 )

instance Exts.IsList (Poly a) where
  type Item (Poly a) = a
  fromList  = fromList
  fromListN = fromListN
  toList    = toList

fromList :: [a] -> Poly a
fromList = Poly . Vector.fromList

fromListN :: Int -> [a] -> Poly a
fromListN = (fmap Poly) . Vector.fromListN

toList :: Poly a -> [a]
toList = Vector.toList . unPoly

degree :: Poly a -> Int
degree p = (Vector.length $ unPoly p) - 1

empty :: Poly a
empty = Poly $ Vector.empty

singleton :: a -> Poly a
singleton = Poly . Vector.singleton

-- | Compose two polynomials. Illustrated:
--   compose f g = h
--   =
--   f(g(x)) = h(x)
compose :: Semiring a => Poly a -> Poly a -> Poly a
compose (Poly x) y = horner y (fmap singleton x)

-- | Compose any number of polynomials of the form
-- f0(f1(f2(...(fN(x))))) into
-- f(x)
collapse :: Semiring a => Vector (Poly a) -> Poly a
collapse = Foldable.foldr compose zero

-- | Evaluate a polynomial.
-- 
-- Given a value x and a function f, compute f(x)
evaluate :: Semiring a => a -> Poly a -> a
evaluate = horner

-- horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

shift, unShift :: Semiring a => Poly a -> Poly a
shift (Poly xs)
  | Vector.null xs = empty
  | otherwise = Poly $ zero `Vector.cons` xs

unShift (Poly xs)
  | Vector.null xs = empty
  | otherwise = Poly $ Vector.unsafeTail xs

scale :: Semiring a => a -> Poly a -> Poly a
scale s = Poly . Vector.map (s *) . unPoly

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

instance Semiring a => Semiring (Poly a) where
  zero = empty
  one  = singleton one

  plus  x y = Poly $ polyPlus  (unPoly x) (unPoly y)
  times x y = Poly $ polyTimes (unPoly x) (unPoly y)

instance Ring a => Ring (Poly a) where
  negate = Poly . Vector.map negate . unPoly
