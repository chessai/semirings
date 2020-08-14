module Data.Euclidean where

import           Numeric.Natural                ( Natural )
import {-# SOURCE #-} Data.Semiring             ( IntegralDomain )
import           Prelude                 hiding ( quot
                                                , rem
                                                , quotRem
                                                )

class GcdDomain a

class GcdDomain a => Euclidean a where
  quotRem :: a -> a -> (a, a)
  quotRem x y = (quot x y, rem x y)
  quot :: a -> a -> a
  quot x y = fst (quotRem x y)
  rem :: a -> a -> a
  rem x y = snd (quotRem x y)
  degree :: a -> Natural
  {-# MINIMAL (quotRem | quot, rem), degree #-}

class Field a

newtype WrappedIntegral a = WrapIntegral { unwrapIntegral :: a }
newtype WrappedFractional a = WrapFractional { unwrapFractional :: a }

instance Integral a => IntegralDomain (WrappedIntegral a)
instance Fractional a => IntegralDomain (WrappedFractional a)
