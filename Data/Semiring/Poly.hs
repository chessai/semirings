{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Semiring.Poly
  ( Poly(..)
  , Poly2(..)
  , Poly3(..)
  , polyJoin
  , collapse
  , composePoly
  , horner
  , vectorTimes
  ) where

import           Control.Applicative (Applicative(liftA2))
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
--import           Data.Maybe
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           GHC.Generics (Generic, Generic1)
import qualified Prelude as P
import           Prelude (($), (.), id)

import Data.Semiring (Semiring(zero,one,plus,times), (+), (*))

polyJoin :: Semiring a => Poly (Poly a) -> Poly a
polyJoin (Poly x) = collapse x

collapse :: Semiring a => Vector (Poly a) -> Poly a
collapse = Foldable.foldr composePoly zero

-- | The type of polynomials in one variable
newtype Poly  a   = Poly  (Vector a)
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor, P.Foldable)

-- | The type of polynomials in two variables
newtype Poly2 a b = Poly2 (Vector (a,b))
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

-- | The type of polynomials in three variables
newtype Poly3 a b c = Poly3 (Vector (a,b,c))
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

instance Applicative Poly where
  pure = Poly . Vector.singleton
  Poly f <*> Poly v = Poly $ Vector.zipWith id f v

composePoly :: Semiring a => Poly a -> Poly a -> Poly a
composePoly (Poly x) y = horner y (P.fmap (Poly . Vector.singleton) x)

--instance P.Monad Poly where
--  p >>= f = polyJoin (P.fmap f p)

-- | Horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

vectorPlus :: Semiring a => Vector a -> Vector a -> Vector a
vectorPlus x y
  = if Vector.null x then y else if Vector.null y then x else
  liftA2 plus x y

vectorTimes :: Semiring a => Vector a -> Vector a -> Vector a
vectorTimes x y
  = if Vector.null x then x else if Vector.null y then y else
  liftA2 times x y

-- | There is no way to do better than /O(n^2)/ for any
-- semiring. There is an algorithm that is /O(n log n)/,
-- but it requires something at least as strong as 'RealFloat'.
polyTimes :: Semiring a => Vector a -> Vector a -> Vector a
polyTimes x y
  = if Vector.null x then x else if Vector.null y then y else
      Vector.cons (times a b) (Vector.map (a *) q + Vector.map (* b) p + (Vector.cons zero (polyTimes p q)))
  where
    a = Vector.unsafeHead x
    b = Vector.unsafeHead y
    p = (\t d -> if Vector.null t then d else t) (Vector.tail x) (Vector.empty)
    q = (\t d -> if Vector.null t then d else t) (Vector.tail y) (Vector.empty)

instance Semiring a => Semiring (Poly a) where
  zero = Poly (Vector.empty)
  one  = Poly (Vector.singleton one)

  plus  (Poly x) (Poly y) = Poly $ vectorPlus  x y
  times (Poly x) (Poly y) = Poly $ polyTimes x y

instance (Semiring a, Semiring b) => Semiring (Poly2 a b) where
  zero = Poly2 (Vector.empty)
  one  = Poly2 (Vector.singleton one)

  plus  (Poly2 x) (Poly2 y) = Poly2 $ vectorPlus  x y
  times (Poly2 x) (Poly2 y) = Poly2 $ polyTimes x y

instance (Semiring a, Semiring b, Semiring c) => Semiring (Poly3 a b c) where
  zero = Poly3 (Vector.empty)
  one  = Poly3 (Vector.singleton one)

  plus  (Poly3 x) (Poly3 y) = Poly3 $ vectorPlus  x y
  times (Poly3 x) (Poly3 y) = Poly3 $ polyTimes x y
