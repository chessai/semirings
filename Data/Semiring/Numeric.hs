{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Semiring.Numeric
  ( Bottleneck(..)
  , Division(..)
  , Lukas(..)
  , Viterbi(..)
  , Log(..)
  ) where

import           Data.Coerce
import           GHC.Generics
import           Foreign.Storable (Storable)

import           Data.Semiring

import Prelude hiding ((+),(-),negate)

type WrapBinary f a = (a -> a -> a) -> f a -> f a -> f a

-- | Useful for some constraint problems.
--
-- @'plus'  = 'max'
--  'times' = 'min'
--  'zero'  = 'minBound'
--  'one'   = 'maxBound'@
newtype Bottleneck a = Bottleneck { getBottleneck :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable)

-- | 
--
-- @'plus'  = 'gcd'
--  'times' = 'lcm'
--  'zero'  = 'zero'
--  'one'   = 'one'@
newtype Division a = Division { getDivision :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable)

-- | <https://en.wikipedia.org/wiki/Semiring Wikipedia>
-- has some information on this. Also
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.6152&rep=rep1&type=pdf this>
-- paper.
--
-- @'plus'   = 'max'
--x 'times' y = 'max' 0 (x '+' y '-' 1)
--'zero'    = 'zero'
--'one'     = 'one'@
newtype Lukas a = Lukas { getLukas :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable)

-- | <https://en.wikipedia.org/wiki/Semiring Wikipedia>
-- has some information on this. Also
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.6152&rep=rep1&type=pdf this>
-- paper. Apparently used for probabilistic parsing.
--
-- @'plus' = 'max'
--'times' = 'times'
--'zero'  = 'zero'
--'one'   = 'one'@
newtype Viterbi a = Viterbi { getViterbi :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable)

-- | Useful for optimising multiplication, or working with large numbers.
--
-- @'times'    = ('plus')
--  x 'plus' y = -('log' ('exp' (-x) '`plus`' 'exp' (-y)))
--'zero'       = 'positiveInfinity'
--'one'        = zero@
newtype Log a = Log { getLog :: a }
  deriving (Eq, Ord, Read, Show, Generic, Generic1, Functor
             ,Foldable)

instance (Bounded a, Ord a) => Semiring (Bottleneck a) where
  plus  = (coerce :: WrapBinary Bottleneck a) max
  times = (coerce :: WrapBinary Bottleneck a) min
  zero  = Bottleneck minBound
  one   = Bottleneck maxBound

instance (Integral a, Semiring a) => Semiring (Division a) where
  plus  = (coerce :: WrapBinary Division a) gcd
  times = (coerce :: WrapBinary Division a) lcm
  zero  = Division zero
  one   = Division one

instance (Ord a, Ring a) => Semiring (Lukas a) where
  plus  = (coerce :: WrapBinary Lukas a) max
  times = (coerce :: WrapBinary Lukas a) (\x y -> max zero (x `plus` y `minus` one))
  zero  = Lukas zero
  one   = Lukas one

instance (Ord a, Semiring a) => Semiring (Viterbi a) where
  plus  = (coerce :: WrapBinary Viterbi a) max
  times = (coerce :: WrapBinary Viterbi a) times
  zero  = Viterbi zero
  one   = Viterbi one

instance (Floating a, Ring a) => Semiring (Log a) where
  zero  = Log zero -- This SHOULD be +Inf
  one   = Log one
  times = (coerce :: WrapBinary Log a) plus
  plus (Log x) (Log y)
    = Log (negate (log (exp (negate x) + exp (negate y))))
