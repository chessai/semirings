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
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE StandaloneDeriving         #-}
#else
{-# LANGUAGE TemplateHaskell            #-}
#endif

module Data.Euclidean
  ( Euclidean(..)
  , Field
  , GcdDomain(..)
  , WrappedIntegral(..)
  , WrappedFractional(..)
  , gcdExt
  ) where

import Prelude hiding (quotRem, quot, rem, divMod, div, mod, gcd, lcm, negate, (*), Int, Word)
import qualified Prelude as P
import Control.Exception (throw, ArithException(..))
import Data.Bits (Bits)
import Data.Complex (Complex(..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Maybe (isJust)
import Data.Ratio (Ratio)
import Data.Semiring (Semiring(..), Ring(..), (*), minus, isZero, Mod2)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C.Types (CFloat, CDouble)

#if !MIN_VERSION_base(4,12,0)
import Language.Haskell.TH.Syntax (Q, Dec, Type)
#endif

import Numeric.Natural

---------------------------------------------------------------------
-- Classes
---------------------------------------------------------------------

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

  default coprime :: a -> a -> Bool
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
--
-- No particular rounding behaviour is expected of 'quotRem'. E. g.,
-- it is not guaranteed to truncate towards zero or towards negative
-- infinity (cf. 'P.divMod'), and remainders are not guaranteed to be non-negative.
-- For a faithful representation of residue classes one can use
-- <http://hackage.haskell.org/package/mod mod> package instead.
class GcdDomain a => Euclidean a where
  {-# MINIMAL (quotRem | quot, rem), degree #-}
  -- | Division with remainder.
  --
  -- prop> \x y -> y == 0 || let (q, r) = x `quotRem` y in x == q * y + r
  quotRem :: a -> a -> (a, a)
  quotRem x y = (quot x y, rem x y)

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

  -- | Euclidean (aka degree, valuation, gauge, norm) function on @a@. Usually @'fromIntegral' '.' 'abs'@.
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

-- | Execute the extended Euclidean algorithm.
-- For elements @a@ and @b@, compute their greatest common divisor @g@
-- and the coefficient @s@ satisfying @as + bt = g@ for some @t@.
gcdExt :: (Eq a, Euclidean a, Ring a) => a -> a -> (a, a)
gcdExt = go one zero
  where
    go s s' r r'
      | r' == zero = (r, s)
      | otherwise  = case quotRem r r' of
        (q, r'') -> go s' (minus s (times q s')) r' r''
{-# INLINABLE gcdExt #-}

-- | 'Field' represents a
-- <https://en.wikipedia.org/wiki/Field_(mathematics) field>,
-- a ring with a multiplicative inverse for any non-zero element.
class (Euclidean a, Ring a) => Field a

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

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

instance Field ()

instance GcdDomain Mod2 where

instance Euclidean Mod2 where
  degree = const 0
  quotRem x y
    | isZero y  = throw DivideByZero
    | otherwise = (x, zero)

instance Field Mod2

-- | Wrapper around 'Integral' with 'GcdDomain'
-- and 'Euclidean' instances.
newtype WrappedIntegral a = WrapIntegral { unwrapIntegral :: a }
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum, Bits)

instance Num a => Semiring (WrappedIntegral a) where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1
  fromNatural = P.fromIntegral

instance Num a => Ring (WrappedIntegral a) where
  negate = P.negate

instance Integral a => GcdDomain (WrappedIntegral a) where
  divide x y = case x `P.quotRem` y of (q, 0) -> Just q; _ -> Nothing
  gcd     = P.gcd
  lcm     = P.lcm
  coprime = coprimeIntegral

instance Integral a => Euclidean (WrappedIntegral a) where
  degree  = P.fromIntegral . abs . unwrapIntegral
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
  fromNatural = P.fromIntegral

instance Num a => Ring (WrappedFractional a) where
  negate = P.negate

instance Fractional a => GcdDomain (WrappedFractional a) where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

instance Fractional a => Euclidean (WrappedFractional a) where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

instance Fractional a => Field (WrappedFractional a)

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

instance Integral a => Field (Ratio a)

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

instance Field Float

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

instance Field Double

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

instance Field CFloat

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

instance Field CDouble

conjQuotAbs :: Field a => Complex a -> Complex a
conjQuotAbs (x :+ y) = x `quot` norm :+ (negate y) `quot` norm
  where
    norm = (x `times` x) `plus` (y `times` y)

instance Field a => GcdDomain (Complex a) where
  divide x y = Just (x `times` conjQuotAbs y)
  gcd        = const $ const one
  lcm        = const $ const one
  coprime    = const $ const True

instance Field a => Euclidean (Complex a) where
  degree      = const 0
  quotRem x y = (quot x y, zero)
  quot x y    = x `times` conjQuotAbs y
  rem         = const $ const zero

instance Field a => Field (Complex a)

#if MIN_VERSION_base(4,12,0)
deriving via (WrappedIntegral Int) instance GcdDomain Int
deriving via (WrappedIntegral Int8) instance GcdDomain Int8
deriving via (WrappedIntegral Int16) instance GcdDomain Int16
deriving via (WrappedIntegral Int32) instance GcdDomain Int32
deriving via (WrappedIntegral Int64) instance GcdDomain Int64
deriving via (WrappedIntegral Integer) instance GcdDomain Integer
deriving via (WrappedIntegral Word) instance GcdDomain Word
deriving via (WrappedIntegral Word8) instance GcdDomain Word8
deriving via (WrappedIntegral Word16) instance GcdDomain Word16
deriving via (WrappedIntegral Word32) instance GcdDomain Word32
deriving via (WrappedIntegral Word64) instance GcdDomain Word64
deriving via (WrappedIntegral Natural) instance GcdDomain Natural
#else
$(let
  deriveGcdDomain :: Q Type -> Q [Dec]
  deriveGcdDomain ty = [d|
      instance GcdDomain $ty where
         gcd     = P.gcd
         lcm     = P.lcm
         coprime = coprimeIntegral
      |]

  in P.concat P.<$> P.traverse deriveGcdDomain
    [[t|Int|]
    ,[t|Int8|]
    ,[t|Int16|]
    ,[t|Int32|]
    ,[t|Int64|]
    ,[t|Integer|]
    ,[t|Word|]
    ,[t|Word8|]
    ,[t|Word16|]
    ,[t|Word32|]
    ,[t|Word64|]
    ,[t|Natural|]
    ])
#endif

#if MIN_VERSION_base(4,12,0)
deriving via (WrappedIntegral Int) instance Euclidean Int
deriving via (WrappedIntegral Int8) instance Euclidean Int8
deriving via (WrappedIntegral Int16) instance Euclidean Int16
deriving via (WrappedIntegral Int32) instance Euclidean Int32
deriving via (WrappedIntegral Int64) instance Euclidean Int64
deriving via (WrappedIntegral Integer) instance Euclidean Integer
deriving via (WrappedIntegral Word) instance Euclidean Word
deriving via (WrappedIntegral Word8) instance Euclidean Word8
deriving via (WrappedIntegral Word16) instance Euclidean Word16
deriving via (WrappedIntegral Word32) instance Euclidean Word32
deriving via (WrappedIntegral Word64) instance Euclidean Word64
deriving via (WrappedIntegral Natural) instance Euclidean Natural
#else
$(let
  deriveEuclidean :: Q Type -> Q [Dec]
  deriveEuclidean ty = [d|
      instance Euclidean $ty where
         degree  = P.fromIntegral . abs
         quotRem = P.quotRem
         quot    = P.quot
         rem     = P.rem
      |]

  in P.concat P.<$> P.traverse deriveEuclidean
    [[t|Int|]
    ,[t|Int8|]
    ,[t|Int16|]
    ,[t|Int32|]
    ,[t|Int64|]
    ,[t|Integer|]
    ,[t|Word|]
    ,[t|Word8|]
    ,[t|Word16|]
    ,[t|Word32|]
    ,[t|Word64|]
    ,[t|Natural|]
    ])
#endif
