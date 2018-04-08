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

module Data.Semiring.Poly
  ( Poly(..)
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

-- | The type of polynomials in one variable
newtype Poly  a   = Poly { unPoly :: Vector a }
  deriving (P.Eq, P.Ord, P.Read, Generic, Generic1,
            P.Functor, P.Foldable)

instance P.Show a => P.Show (Poly a) where
  show p = "fromList " P.++ (P.show $ unPoly p)

instance Exts.IsList (Poly a) where
  type Item (Poly a) = a
  fromList  = fromList
  fromListN = fromListN
  toList    = toList

fromList :: [a] -> Poly a
fromList = Poly . Vector.fromList

fromListN :: Int -> [a] -> Poly a
fromListN = (P.fmap Poly) . Vector.fromListN

toList :: Poly a -> [a]
toList = Vector.toList . unPoly

degree :: Poly a -> Int
degree p = (Vector.length $ unPoly p) - 1

empty :: Poly a
empty = Poly $ Vector.empty

singleton :: a -> Poly a
singleton = Poly . Vector.singleton

-- | Lazily compose two polynomials. Illustrated:
--   f(g(x)) = h(x)
compose :: Semiring a => Poly a -> Poly a -> Poly a
compose (Poly x) y = horner y (P.fmap singleton x)

-- | Compose any number of polynomials of the form
-- f0(f1(f2(...(fN(x))))) into
-- f(x)
collapse :: Semiring a => Vector (Poly a) -> Poly a
collapse = Foldable.foldr compose zero

-- | Horner's scheme for evaluating a polynomial in a semiring
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

collinear :: (Eq a, Semiring a) => Poly a -> Poly a -> Bool
collinear (Poly f) (Poly g)
  | (Vector.length f == 2 && Vector.length g == 2)
      = Vector.unsafeHead f == Vector.unsafeHead g
  | (Vector.length f == 1 && Vector.length g == 1)
      = Vector.unsafeHead f == Vector.unsafeHead g
  | otherwise = False

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

instance Semiring a => Semiring (Poly a) where
  zero = empty
  one  = singleton one

  plus  x y = Poly $ polyPlus  (unPoly x) (unPoly y)
  times x y = Poly $ polyTimes (unPoly x) (unPoly y)

instance Ring a => Ring (Poly a) where
  negate = Poly . Vector.map negate . unPoly
