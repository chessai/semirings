{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}

module Data.Semiring.Fib
  ( Fib(..)
  , getPhi
  , fib
  ) where

import           Control.Monad.Zip (MonadZip(..))
import           GHC.Generics (Generic, Generic1)

import           Data.Semiring

import Prelude hiding (Num(..),(+),(*),(-),(^),negate)

-- | 'Fib a b' represents a number 'aφ+b'
--   in Z[x] mod x^2 - x - 1
--
--   @'zero' = Fib zero zero
--   'one'   = Fib  one zero
--   'Fib' a b `plus`  'Fib' c d = 'Fib' (a + c) (b + d)
--   'Fib' a b `times` 'Fib' c d = 'Fib' (a*(c + d) + b*c) (a*c + b*d)@
data Fib a = Fib a a
  deriving ( Eq
           , Foldable
           , Functor
#if MIN_VERSION_base(4,6,0)
           , Generic
           , Generic1
#endif
           , Read
           , Show
           , Traversable
           )

-- | Given 'Fib a b', returns 'a'.
getPhi :: Fib a -> a
getPhi (Fib a _) = a
{-# INLINE getPhi #-}

-- | Compute the nth Fibonacci number in O(log n)
-- Note that this extends to negative Fibonacci.
fib :: Ring a => Integer -> a
fib n
  | n >= 0 = getPhi (Fib one zero ^ n)
  | otherwise = getPhi (Fib one (negate one) ^ negate n)

instance Semiring a => Semiring (Fib a) where
  zero = Fib zero zero
  one  = Fib  one zero
  Fib a b `plus`  Fib c d = Fib (a + c) (b + d)
  -- exploits φ^2 = φ+1 
  Fib a b `times` Fib c d = Fib (a*(c + d) + b*c) (a*c + b*d)

instance Applicative Fib where
  pure a = Fib a a
  Fib a b <*> Fib c d = Fib (a c) (b d)

instance Monad Fib where
#if !MIN_VERSION_base(4,11,0)
  return a = Fib a a
#endif
  Fib a b >>= f = Fib a' b' where
    Fib a' _ = f a
    Fib _ b' = f b

instance MonadZip Fib where
  mzipWith f (Fib a b) (Fib c d) = Fib (f a c) (f b d)
  munzip (Fib (a,b) (c,d)) = (Fib a c, Fib b d)

two, five :: Semiring a => a
two  = one + one
five = one + one + one + one + one
{-# INLINE two  #-}
{-# INLINE five #-}

-- First we check for a quick exit if both results are consistent.
-- It is only when 'sign a /= sign b' in 'aφ+b' that there is a
-- problem - which is bigger, aφ or b?
-- We can write a recursive solution that computes repeated remainders,
-- 'gcd'-style, but we hit the worst case possible for the usual gcd
-- algorithm for Fib 1 (-1) ^ n. This is <https://www.cut-the-knot.org/blue/LamesTheorem.shtml Lamé's Theorem> showing up in the wild.
-- So instead, in the above instance we convert aφ+b to e√5 + f and
-- compare the squares instead. This works nicely even when 'a' is 'Double'
-- and is capable of representing √5 on its own.
instance (Ord a, Ring a) => Ord (Fib a) where
  compare (Fib a b) (Fib c d) = case compare a c of
    LT | b <= d    -> LT
       | otherwise -> go compare (a - c) (b - d)
    EQ -> compare b d
    GT | b >= d    -> GT
       | otherwise -> go (flip compare) (a - c) (b - d)
   where
     go :: Ring a => (a -> a -> Ordering) -> a -> a -> Ordering
     go k e f = k (square (e + two*f)) (five * square e)
     square :: Semiring a => a -> a
     square x = x * x
