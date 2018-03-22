{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

--{-# OPTIONS_GHC -Wall #-}

module Data.Semiring.Poly
  ( Poly(..)
  , Poly2(..)
  , Poly3(..)
  , polyJoin
  , collapse
  , composePoly
  , horner
  ) where

import           Control.Applicative (Applicative(liftA2))
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
--import           Data.Maybe
import           Data.Monoid (mempty)
import           Data.Primitive.Array (Array)
import qualified Data.Primitive.Array as Array
import           GHC.Generics (Generic, Generic1)
import qualified Prelude as P
import           Prelude (($), (.), id)

import Data.Ring (Ring(..), (-), minus)
import Data.Semiring (Semiring(zero,one,plus,times), (+), (*))

polyJoin :: Semiring a => Poly (Poly a) -> Poly a
polyJoin (Poly x) = collapse x

collapse :: Semiring a => Array (Poly a) -> Poly a
collapse = Foldable.foldr composePoly zero

-- | The type of polynomials in one variable
newtype Poly  a   = Poly  (Array a)
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor, P.Foldable)

-- | The type of polynomials in two variables
newtype Poly2 a b = Poly2 (Array (a,b))
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

-- | The type of polynomials in three variables
newtype Poly3 a b c = Poly3 (Array (a,b,c))
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

composePoly :: Semiring a => Poly a -> Poly a -> Poly a
composePoly (Poly x) y = horner y (P.fmap (Poly . arraySingleton) x)

arraySingleton :: a -> Array a
arraySingleton = P.pure

infixl 1 `bind`

bind :: forall a b. Semiring b => Poly a -> (a -> Poly b) -> Poly b
p `bind` f = polyJoin (P.fmap f p)

-- | Horner's scheme for evaluating a polynomial in a semiring
horner :: (Semiring a, Foldable t) => a -> t a -> a
horner x = Foldable.foldr (\c val -> c + x * val) zero

arrayPlus :: Semiring a => Array a -> Array a -> Array a
arrayPlus x y = x

polyTimes :: Semiring a => Array a -> Array a -> Array a
polyTimes x y = x

instance Semiring a => Semiring (Poly a) where
  zero = Poly mempty
  one  = Poly $ arraySingleton one

  plus  (Poly x) (Poly y) = Poly $ arrayPlus  x y
  times (Poly x) (Poly y) = Poly $ polyTimes x y

instance (Semiring a, Semiring b) => Semiring (Poly2 a b) where
  zero = Poly2 mempty
  one  = Poly2 $ arraySingleton one

  plus  (Poly2 x) (Poly2 y) = Poly2 $ arrayPlus  x y
  times (Poly2 x) (Poly2 y) = Poly2 $ polyTimes x y

instance (Semiring a, Semiring b, Semiring c) => Semiring (Poly3 a b c) where
  zero = Poly3 mempty
  one  = Poly3 $ arraySingleton one

  plus  (Poly3 x) (Poly3 y) = Poly3 $ arrayPlus  x y
  times (Poly3 x) (Poly3 y) = Poly3 $ polyTimes x y
