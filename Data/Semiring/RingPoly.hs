{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

--{-# OPTIONS_GHC -Wall #-}

module Data.Semiring.RingPoly
  ( RingPoly(..)
  , collapse
  , compose
  , horner
  , degree
  , fromList
  , fromListN
  , toList
  , empty
  , singleton
  ) where

import           Data.Bool (Bool(..), (&&))
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Ord (Ordering(..), compare)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified GHC.Exts as Exts
import           GHC.Generics (Generic, Generic1)
import qualified Prelude as P
import           Prelude (Eq(..))
import           Prelude (($), (.), otherwise)
import           Prelude (Int, IO)

import Data.Semiring

-- | The type of polynomials in one variable,
--   but the underlying type is generally treated
--   with the stronger constraint of 'Ring'.
newtype RingPoly  a   = RingPoly { unRingPoly :: Vector a }
  deriving (P.Eq, P.Ord, P.Read, Generic, Generic1,
            P.Functor, P.Foldable)

instance P.Show a => P.Show (RingPoly a) where
  show p = "fromList " P.++ (P.show $ unRingPoly p)

instance Exts.IsList (RingPoly a) where
  type Item (RingPoly a) = a
  fromList  = fromList
  fromListN = fromListN
  toList    = toList

fromList :: [a] -> RingPoly a
fromList = RingPoly . Vector.fromList

fromListN :: Int -> [a] -> RingPoly a
fromListN = (P.fmap RingPoly) . Vector.fromListN

toList :: RingPoly a -> [a]
toList = Vector.toList . unRingPoly

degree :: RingPoly a -> Int
degree p = (Vector.length $ unRingPoly p) - 1

empty :: RingPoly a
empty = RingPoly $ Vector.empty

singleton :: a -> RingPoly a
singleton = RingPoly . Vector.singleton

-- | Lazily compose two polynomials. Illustrated:
--   f(g(x)) = h(x)
compose :: Semiring a => RingPoly a -> RingPoly a -> RingPoly a
compose (RingPoly x) y = horner y (P.fmap singleton x)

-- | Compose any number of polynomials of the form
-- f0(f1(f2(...(fN(x))))) into
-- f(x)
collapse :: Semiring a => Vector (RingPoly a) -> RingPoly a
collapse = Foldable.foldr compose zero

-- | Horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

shift, unShift :: Semiring a => RingPoly a -> RingPoly a
shift (RingPoly xs)
  | Vector.null xs = empty
  | otherwise = RingPoly $ zero `Vector.cons` xs

unShift (RingPoly xs)
  | Vector.null xs = empty
  | otherwise = RingPoly $ Vector.unsafeTail xs

scale :: Semiring a => a -> RingPoly a -> RingPoly a
scale s = RingPoly . Vector.map (s *) . unRingPoly

collinear :: (Eq a, Semiring a) => RingPoly a -> RingPoly a -> Bool
collinear (RingPoly f) (RingPoly g)
  | (Vector.length f == 2 && Vector.length g == 2)
      = Vector.unsafeHead f == Vector.unsafeHead g
  | (Vector.length f == 1 && Vector.length g == 1)
      = Vector.unsafeHead f == Vector.unsafeHead g
  | otherwise = False

polyTimesR :: Ring a => Vector a -> Vector a -> Vector a
polyTimesR signal kernel
  | Vector.null signal = Vector.empty
  | Vector.null kernel = Vector.empty
  | otherwise = Vector.generate (slen + klen - 1) f
  where
    f _ = zero

    !slen = Vector.length signal
    !klen = Vector.length kernel

polyMulti2 :: Ring a => Vector a -> Vector a -> Vector a
polyMulti2 xs ys =
  (u + (y - u - z)) * (len `div` 2) + z * (2 * len `div` 2) 
  where
    y = polyMulti2 (a0 + a1) (b0 + b1)
    u = polyMulti2 a0 b0
    z = polyMulti2 a1 b1
    a0 = Vector.generate pow1 f
    a1 = Vector.generate pow2 f
    b0 = Vector.generate pow1 f
    b1 = Vector.generate pow2 f
    f _ = zero
   
    !pow1 = len `div` 2 - 1
    !pow2 = len - len `div` 2
    !len  = slen + klen - 1 
    !slen = Vector.length xs
    !klen = Vector.length ys

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
        !kmin = P.max 0 (n - (klen - 1))
        !kmax = P.min n (slen - 1)
    !slen = Vector.length signal
    !klen = Vector.length kernel

instance Semiring a => Semiring (RingPoly a) where
  zero = empty
  one  = singleton one

  plus  x y = RingPoly $ polyPlus  (unRingPoly x) (unRingPoly y)
  times x y = RingPoly $ polyTimes (unRingPoly x) (unRingPoly y)

instance Ring a => Ring (RingPoly a) where
  negate = RingPoly . Vector.map negate . unRingPoly
