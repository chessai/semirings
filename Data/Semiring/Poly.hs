{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
  ( 
#if defined(VERSION_vector) 
    -- * Polynomial type
    Poly(..)
 
    -- ** Construction
  , empty
  , singleton
  , shift
  , shiftN
  , unShift
  , unShiftN
  , scale
  , map

    -- ** Composition 
  , collapse
  , compose
  
    -- ** Evaluation 
  , evaluate 
 
    -- ** Querying 
  , degree
  , nth

    -- ** List Conversion
  , fromList
  , fromListN
  , toList
#endif
  ) where

#if defined(VERSION_vector)
import           Control.Applicative (Applicative, Alternative)
import           Control.DeepSeq (NFData)
import           Control.Monad (MonadPlus)
import           Control.Monad.Zip (MonadZip)
import           Data.Data (Data)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Data.Maybe (fromMaybe)
import           Data.Ord (Ordering(..), compare)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified GHC.Exts as Exts
import           GHC.Generics (Generic, Generic1)
import           Prelude hiding (Num(..), map)

import Data.Semiring
import Data.Star

-- | The type of polynomials in one variable.
-- Backed by 'Data.Vector.Vector'.
newtype Poly a = Poly { unPoly :: Vector a }
  deriving ( Alternative
           , Applicative
           , Data
           , Eq
           , Foldable
           , Functor
#if MIN_VERSION_base(4,6,0)
           , Generic
           , Generic1
#endif
           , Monad
           , MonadPlus 
           , MonadZip
           , Monoid
           , NFData
           , Ord  
           , Read
           , Ring
           , Semigroup
           , Show
           , Traversable
#if MIN_VERSION_base(4,9,0)
           , Eq1, Ord1, Read1, Show1
#endif
           )

#if MIN_VERSION_base(4,7,0)
instance Exts.IsList (Poly a) where
  type Item (Poly a) = a
  fromList  = fromList
  fromListN = fromListN
  toList    = toList
#endif

-- this is horribly inefficient.
instance (Star a) => Star (Poly a) where
  star (Poly v)
    | Vector.null v = one
    | otherwise = Poly r
    where
      r = Vector.cons xst $ Vector.map (xst *) (Vector.unsafeTail v * r)
      xst = star (Vector.unsafeHead v)

fromList :: [a] -> Poly a
fromList = Poly . Vector.fromList

fromListN :: Int -> [a] -> Poly a
fromListN = (fmap Poly) . Vector.fromListN

toList :: Poly a -> [a]
toList = Vector.toList . unPoly

-- | /O(1)/. What is the degree of the Polynomial?
degree :: Poly a -> Int
degree (Poly p) = Vector.length p

-- | /O(1)/. What is the nth coefficient of the Polynomial?
-- This query treats the constant coefficient of the polynomial
-- as the 0th coefficient.
nth :: Semiring a => Int -> Poly a -> a
nth n (Poly v) = fromMaybe zero (v' Vector.!? n)
  where v' = Vector.reverse v

-- | /O(1)/. Construct an empty Polynomial. 
empty :: Poly a
empty = Poly $ Vector.empty

-- | /O(1)/. Construct a singleton Polynomial.
singleton :: a -> Poly a
singleton = Poly . Vector.singleton

-- | Compose two polynomials. Illustrated:
--   
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

-- | /O(n)/. Evaluate a polynomial.
-- 
-- Given a value x and a function f, compute f(x)
evaluate :: Semiring a => a -> Poly a -> a
evaluate = horner

-- horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

-- | /O(n)/. Multiply all terms in the polynomial by 'x'.
--
-- Example:
--
-- shift $ fromList [1,2,3] = fromList [1,2,3,0],
--
-- or
--
-- shift (x^2 + 2x + 3) = (x^3 + 2x^2 + 3x + 0)
shift :: Semiring a => Poly a -> Poly a
shift (Poly xs)
  | Vector.null xs = empty
  | otherwise = Poly $ Vector.snoc xs zero

-- | /O(N + 1)/. Perform 'shift' on a polynomial 'N' times.
--
-- Example:
--
-- shiftN 3 $ fromList [1,2,3] = fromList [1,2,3,0,0,0]
--
-- or
--
-- shiftN 3 (x^2 + 2x + 3) = (x^5 + 2x^4 + 3x^3 + 0x^2 + 0x + 0)
shiftN :: Semiring a => Int -> Poly a -> Poly a
shiftN d (Poly v) = Poly v'
  where
    l = Vector.length v
    v' = Vector.generate (d + 1) (\i -> if i < l then Vector.unsafeIndex v i else zero)

-- | /O(n)/. Divide all terms in the polynomial by 'x'.
--
-- Example:
--
-- unShift $ fromList [1,2,3] = fromList [2,3]
--
-- or
--
-- unShift (x^2 + 2x + 3) = (2x + 3)
unShift :: Semiring a => Poly a -> Poly a
unShift p@(Poly v)
  | Vector.null v = p
  | otherwise = Poly $ Vector.unsafeTail v

-- | /O(n - N)/. Perform 'unShift' on a polynomial 'N' times.
--
-- Example:
--
-- unshiftN 3 $ fromList [1,2,3] = []
--
-- or
--
-- unShiftN 3 (x^2 + 2x + 3) = []
unShiftN :: Semiring a => Int -> Poly a -> Poly a
unShiftN d (Poly v)
  | d >= l = empty
  | otherwise = Poly $ Vector.generate (l - d) (\i -> Vector.unsafeIndex v (i + d))
  where
    l = Vector.length v

-- | /O(n)/. Scale a polynomial by a constant factor.
--
-- Example:
--
-- scale 2 $ fromList [1,2,3] = fromList [2,4,6],
--
-- or
--
-- scale 2 (x^2 + 2x + 3) = (2x^2 + 4x + 6)
scale :: Semiring a => a -> Poly a -> Poly a
scale s = Poly . Vector.map (s *) . unPoly

-- | /O(n)/. Map a function over the coefficients of a Polynomial.
--
map :: (Semiring a, Semiring b) => (a -> b) -> Poly a -> Poly b
map f = Poly . Vector.map f . unPoly

polyPlus, polyTimes :: Semiring a => Vector a -> Vector a -> Vector a
polyPlus xs ys =
  case compare (Vector.length xs) (Vector.length ys) of
    EQ -> Vector.zipWith (+) xs ys
    LT -> Vector.unsafeAccumulate (+) ys (Vector.indexed xs)
    GT -> Vector.unsafeAccumulate (+) xs (Vector.indexed ys)
polyTimes !signal !kernel
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

#endif
