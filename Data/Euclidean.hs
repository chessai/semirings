-- |
-- Module:      Data.Euclidean
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}

module Data.Euclidean
  ( Euclidean(..)
  , GcdDomain(..)
  , WrappedIntegral(..)
  , WrappedFractional(..)
  ) where

import Prelude hiding (quotRem, quot, rem, divMod, div, mod, gcd, lcm, (*))
import qualified Prelude as P
import Data.Bits
import Data.Complex
import Data.Maybe
import Data.Ratio
import Data.Semiring
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals
import Numeric.Natural

-- | 'GcdDomain' represents a
-- <https://en.wikipedia.org/wiki/GCD_domain GCD domain>.
-- This is a domain, where GCD can be defined,
-- but which does not necessarily allow a well-behaved
-- division with remainder (as in 'Euclidean' domains).
--
-- For example, there is no way to define 'rem' over
-- polynomials with integer coefficients such that
-- remainder is always "smaller" than divisor. However,
-- 'gcd' is still definable, just not by means of
-- Euclidean algorithm.
--
-- All methods of 'GcdDomain' have default implementations
-- in terms of 'Euclidean'. So most of the time
-- it is enough to write:
--
-- > instance GcdDomain Foo
-- > instance Euclidean Foo where
-- >   quotRem = ...
-- >   degree  = ...
class Semiring a => GcdDomain a where

  -- | Division without remainder.
  --
  -- prop> \x y -> (x * y) `divide` y == Just x
  -- prop> \x y -> maybe True (\z -> x == z * y) (x `divide` y)
  divide :: a -> a -> Maybe a

  default divide :: (Eq a, Euclidean a) => a -> a -> Maybe a
  divide x y = let (q, r) = quotRem x y in
    if isZero r then Just q else Nothing

  -- | Greatest common divisor. Must satisfy
  --
  -- prop> \x y -> isJust (x `divide` gcd x y) && isJust (y `divide` gcd x y)
  -- prop> \x y z -> isJust (gcd (x * z) (y * z) `divide` z)
  gcd :: a -> a -> a

  default gcd :: (Eq a, Euclidean a) => a -> a -> a
  gcd a b
    | isZero b  = a
    | otherwise = gcd b (a `rem` b)

  -- | Lowest common multiple. Must satisfy
  --
  -- prop> \x y -> isJust (lcm x y `divide` x) && isJust (lcm x y `divide` y)
  -- prop> \x y z -> isNothing (z `divide` x) || isNothing (z `divide` y) || isJust (z `divide` lcm x y)
  lcm :: a -> a -> a

  default lcm :: Eq a => a -> a -> a
  lcm a b
    | isZero a || isZero b = zero
    | otherwise = case a `divide` gcd a b of
      Nothing -> error "lcm: violated gcd invariant"
      Just c  -> c * b

  -- | Test whether two arguments are
  -- <https://en.wikipedia.org/wiki/Coprime_integers coprime>.
  -- Must match its default definition:
  --
  -- prop> \x y -> coprime x y == isJust (1 `divide` gcd x y)
  coprime :: a -> a -> Bool

  default coprime :: Eq a => a -> a -> Bool
  coprime x y = isJust (one `divide` gcd x y)

infixl 7 `divide`

-- | Informally speaking, 'Euclidean' is a superclass of 'Integral',
-- lacking 'toInteger', which allows to define division with remainder
-- for a wider range of types, e. g., complex integers
-- and polynomials with rational coefficients.
--
-- 'Euclidean' represents a
-- <https://en.wikipedia.org/wiki/Euclidean_domain Euclidean domain>
-- endowed by a given Euclidean function 'degree'.
class GcdDomain a => Euclidean a where
  -- | Division with remainder.
  --
  -- prop> \x y -> y == 0 || let (q, r) = x `quotRem` y in x == q * y + r
  quotRem :: a -> a -> (a, a)

  -- | Division. Must match its default definition:
  --
  -- prop> \x y -> quot x y == fst (quotRem x y)
  quot :: a -> a -> a
  quot x y = fst (quotRem x y)

  -- | Remainder. Must match its default definition:
  --
  -- prop> \x y -> rem x y == snd (quotRem x y)
  rem :: a -> a -> a
  rem x y = snd (quotRem x y)

  -- | Euclidean (aka degree, valuation, gauge, norm) function on 'a'. Usually 'fromIntegral' . 'abs'.
  --
  -- 'degree' is rarely used by itself. Its purpose
  -- is to provide an evidence of soundness of 'quotRem'
  -- by testing the following property:
  --
  -- prop> \x y -> y == 0 || let (q, r) = x `quotRem` y in (r == 0 || degree r < degree y)
  degree :: a -> Natural

infixl 7 `quot`
infixl 7 `rem`

coprimeIntegral :: Integral a => a -> a -> Bool
coprimeIntegral x y = (odd x || odd y) && P.gcd x y == 1

instance GcdDomain () where
  divide  = const $ const (Just ())
  gcd     = const $ const ()
  lcm     = const $ const ()
  coprime = const $ const True

instance Euclidean () where
  degree  = const 0
  quotRem = const $ const ((), ())
  quot    = const $ const ()
  rem     = const $ const ()

-- | Wrapper around 'Integral' with 'GcdDomain'
-- and 'Euclidean' instances.
newtype WrappedIntegral a = WrapIntegral { unwrapIntegral :: a }
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum, Bits)

instance Num a => Semiring (WrappedIntegral a) where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1
  fromNatural = fromIntegral

instance Integral a => GcdDomain (WrappedIntegral a) where
  gcd     = P.gcd
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Integral a => Euclidean (WrappedIntegral a) where
  degree  = fromIntegral . abs . unwrapIntegral
  quotRem = P.quotRem
  quot    = P.quot
  rem     = P.rem

instance GcdDomain Int where
#if MIN_VERSION_integer_gmp(0,5,1)
  gcd (I# x) (I# y) = I# (gcdInt x y)
#else
  gcd     = P.gcd
#endif
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Euclidean Int where
  degree  = fromIntegral . abs
  quotRem = P.quotRem
  quot    = P.quot
  rem     = P.rem

instance GcdDomain Word where
#if MIN_VERSION_integer_gmp(1,0,0)
  gcd (W# x) (W# y) = W# (gcdWord x y)
#else
  gcd     = P.gcd
#endif
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Euclidean Word where
  degree  = fromIntegral
  quotRem = P.quotRem
  quot    = P.quot
  rem     = P.rem

instance GcdDomain Integer where
  gcd     = gcdInteger
  lcm     = lcmInteger
  coprime = coprimeIntegral

instance Euclidean Integer where
  degree  = fromInteger . abs
  quotRem = P.quotRem
  quot    = P.quot
  rem     = P.rem

instance GcdDomain Natural where
  gcd     = P.gcd
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Euclidean Natural where
  degree  = id
  quotRem = P.quotRem
  quot    = P.quot
  rem     = P.rem

-- | Wrapper around 'Fractional'
-- with trivial 'GcdDomain'
-- and 'Euclidean' instances.
newtype WrappedFractional a = WrapFractional { unwrapFractional :: a }
  deriving (Eq, Ord, Show, Num, Fractional)

instance Num a => Semiring (WrappedFractional a) where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1
  fromNatural = fromIntegral

instance Fractional a => Ring (WrappedFractional a) where
  negate = P.negate

instance (Eq a, Fractional a) => GcdDomain (WrappedFractional a) where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance (Eq a, Fractional a) => Euclidean (WrappedFractional a) where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance Integral a => GcdDomain (Ratio a) where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance Integral a => Euclidean (Ratio a) where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance GcdDomain Float where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance Euclidean Float where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance GcdDomain Double where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance Euclidean Double where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance GcdDomain CFloat where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance Euclidean CFloat where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance GcdDomain CDouble where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance Euclidean CDouble where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance (Eq a, Fractional a, Ring a) => GcdDomain (Complex a) where
  divide _ (0 :+ 0) = Nothing
  divide z (x :+ y)
    | d == 0        = Nothing
    | otherwise     = Just (z `times` ((x / d) :+ (-y / d)))
    where
      d = x `times` x `plus` y `times` y
  gcd               = const $ const (1 :+ 0)
  lcm               = const $ const (1 :+ 0)
  coprime           = const $ const True

instance (Eq a, Fractional a, Ring a) => Euclidean (Complex a) where
  degree      = const 0
  quotRem x y = case x `divide` y of
    Nothing -> (0 P./ 0 :+ 0 P./ 0, 0 :+ 0)
    Just z  -> (z, 0 :+ 0)
  quot x y    = case x `divide` y of
    Nothing -> 0 P./ 0 :+ 0 P./ 0
    Just z  -> z
  rem         = const $ const (0 :+ 0)
