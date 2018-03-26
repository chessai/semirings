{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Semiring.Poly
  ( Poly(..)
  , Poly2(..)
  , Poly3(..)
  , polyBind 
  , polyJoin
  , collapse
  , composePoly
  , horner
  ) where

import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Ord (Ordering(..), compare)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           GHC.Generics (Generic, Generic1)
import qualified Prelude as P
import           Prelude (($), (.), otherwise)

--import Data.Ring (Ring(..), (-), minus)
import Data.Semiring (Semiring(zero,one,plus,times), (+), (*))

polyJoin :: Semiring a => Poly (Poly a) -> Poly a
polyJoin (Poly x) = collapse x

collapse :: Semiring a => Vector (Poly a) -> Poly a
collapse = Foldable.foldr composePoly zero

-- | The type of polynomials in one variable
newtype Poly  a   = Poly { unPoly :: Vector a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor, P.Foldable)

-- | The type of polynomials in two variables
newtype Poly2 a b = Poly2 { unPoly2 :: Vector (a,b) }
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

-- | The type of polynomials in three variables
newtype Poly3 a b c = Poly3 { unPoly3 :: Vector (a,b,c) }
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

empty :: Poly a
empty = Poly $ Vector.empty

empty2 :: Poly2 a b
empty2 = Poly2 $ Vector.empty

empty3 :: Poly3 a b c
empty3 = Poly3 $ Vector.empty

singleton :: a -> Poly a
singleton = Poly . Vector.singleton

singleton2 :: (a,b) -> Poly2 a b
singleton2 = Poly2 . Vector.singleton

singleton3 :: (a,b,c) -> Poly3 a b c
singleton3 = Poly3 . Vector.singleton

composePoly :: Semiring a => Poly a -> Poly a -> Poly a
composePoly (Poly x) y = horner y (P.fmap (Poly . Vector.singleton) x)

infixl 1 `polyBind`

polyBind :: forall a b. Semiring b => Poly a -> (a -> Poly b) -> Poly b
p `polyBind` f = polyJoin (P.fmap f p)

-- | Horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

polyPlus, polyTimes :: Semiring a => Vector a -> Vector a -> Vector a
polyPlus xs ys =
  case compare (Vector.length xs) (Vector.length ys) of
    EQ -> Vector.zipWith (+) xs ys
    LT -> Vector.unsafeAccumulate (+) ys (Vector.indexed xs)
    GT -> Vector.unsafeAccumulate (+) xs (Vector.indexed ys)
polyTimes signal kernel
  | Vector.null signal = Vector.empty
  | Vector.null kernel = Vector.empty
  | otherwise = Vector.generate (slen P.+ klen P.- 1) f
  where
    f n = Foldable.foldl'
      (\a k ->
        a +
        Vector.unsafeIndex signal k *
        Vector.unsafeIndex kernel (n P.- k)) zero [kmin .. kmax]
      where
        !kmin = P.max 0 (n P.- (klen P.- 1))
        !kmax = P.min n (slen P.- 1)
    !slen = Vector.length signal
    !klen = Vector.length kernel

instance Semiring a => Semiring (Poly a) where
  zero = empty
  one  = singleton one

  plus x y = Poly $ polyPlus  (unPoly x) (unPoly y)
  times x y = Poly $ polyTimes (unPoly x) (unPoly y)

instance (Semiring a, Semiring b) => Semiring (Poly2 a b) where
  zero = empty2
  one  = singleton2 one

  plus  x y = Poly2 $ polyPlus  (unPoly2 x) (unPoly2 y)
  times x y = Poly2 $ polyTimes (unPoly2 x) (unPoly2 y)

instance (Semiring a, Semiring b, Semiring c) => Semiring (Poly3 a b c) where
  zero  = empty3
  one   = singleton3 one

  plus  x y = Poly3 $ polyPlus  (unPoly3 x) (unPoly3 y)
  times x y = Poly3 $ polyTimes (unPoly3 x) (unPoly3 y)
