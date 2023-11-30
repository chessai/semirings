{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia                #-}
#else
{-# LANGUAGE TemplateHaskell            #-}
#endif

-----------------------------------------------------------------------------
-- |
-- A class for semirings (types with two binary operations, one commutative and one associative, and two respective identities), with various general-purpose instances.
--
-----------------------------------------------------------------------------

module Data.Semiring
  ( -- * Semiring typeclass
    Semiring(..)
  , (+)
  , (*)
  , (^)
  , foldMapP
  , foldMapT
  , sum
  , product
  , sum'
  , product'
  , isZero
  , isOne

    -- * Types
  , Add(..)
  , Mul(..)
  , WrappedNum(..)
  , Mod2(..)
#if defined(VERSION_containers)
  , IntSetOf(..)
  , IntMapOf(..)
#endif

    -- * Ring typeclass
  , Ring(..)
  , fromInteger
  , fromIntegral
  , minus
  , (-)
  ) where

import           Control.Applicative (Applicative(..), Const(..), liftA2)
import           Data.Bits (Bits (xor))
import           Data.Bool (Bool(..), (||), (&&), otherwise)
import           Data.Coerce (Coercible, coerce)
import           Data.Complex (Complex(..))
import           Data.Eq (Eq(..))
import           Data.Fixed (Fixed, HasResolution)
import           Data.Foldable (Foldable(foldMap))
import qualified Data.Foldable as Foldable
import           Data.Function ((.), const, id)
#if defined(VERSION_unordered_containers) || defined(VERSION_containers)
import           Data.Function (flip)
#endif
import           Data.Functor (Functor(..))
#if MIN_VERSION_base(4,12,0)
import           Data.Functor.Contravariant (Predicate(..), Equivalence(..), Op(..))
#endif
import           Data.Functor.Identity (Identity(..))
#if defined(VERSION_unordered_containers)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
#endif
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Maybe (Maybe(..))
#if MIN_VERSION_base(4,12,0)
import           Data.Monoid (Ap(..))
#endif
#if defined(VERSION_containers)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map)
import qualified Data.Map as Map
#endif
import           Data.Monoid (Monoid(..), Dual(..))
import           Data.Ord (Ord((<)), (>=))
import           Data.Ord (Down(..))
import           Data.Proxy (Proxy(..))
import           Data.Ratio (Ratio, Rational, (%))
import           Data.Semigroup (Semigroup ((<>), stimes))
#if defined(VERSION_containers)
import           Data.Set (Set)
import qualified Data.Set as Set
#endif
import           Data.Traversable (Traversable)
import           Data.Typeable (Typeable)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Foreign.C.Types
  (CChar, CClock, CDouble, CFloat, CInt,
   CIntMax, CIntPtr, CLLong, CLong,
   CPtrdiff, CSChar, CSUSeconds, CShort,
   CSigAtomic, CSize, CTime, CUChar, CUInt,
   CUIntMax, CUIntPtr, CULLong, CULong,
   CUSeconds, CUShort, CWchar)
import           Foreign.Ptr (IntPtr, WordPtr)
import           Foreign.Storable (Storable)
import           GHC.Enum (Enum, Bounded)
import           GHC.Err (error)
import           GHC.Float (Float, Double)
import           GHC.Generics (Generic,Generic1)
import           GHC.IO (IO)
import qualified GHC.Num as Num
import           GHC.Read (Read)
import           GHC.Real (Integral, Fractional, Real, RealFrac)
import qualified GHC.Real as Real
import           GHC.Show (Show)
import           Numeric.Natural (Natural)
import           Prelude (Integer, Ordering(..), compare, even, quot)

#if !MIN_VERSION_base(4,12,0)
import           Language.Haskell.TH.Syntax (Q, Dec, Type)
import qualified Prelude as P
#endif

#ifdef mingw32_HOST_OS
#define HOST_OS_WINDOWS 1
#else
#define HOST_OS_WINDOWS 0
#endif

#if !HOST_OS_WINDOWS
#include "HsBaseConfig.h"
import           System.Posix.Types (
#ifdef HTYPE_CC_T
                 CCc(..),
#endif
#ifdef HTYPE_DEV_T
                 CDev(..),
#endif
#ifdef HTYPE_GID_T
                 CGid(..),
#endif
#ifdef HTYPE_INO_T
                 CIno(..),
#endif
#ifdef HTYPE_MODE_T
                 CMode(..),
#endif
#ifdef HTYPE_NLINK_T
                 CNlink(..),
#endif
#ifdef HTYPE_OFF_T
                 COff(..),
#endif
#ifdef HTYPE_PID_T
                 CPid(..),
#endif
#ifdef HTYPE_RLIM_T
                 CRLim(..),
#endif
#ifdef HTYPE_SPEED_T
                 CSpeed(..),
#endif
#ifdef HTYPE_SSIZE_T
                 CSsize(..),
#endif
#ifdef HTYPE_TCFLAG_T
                 CTcflag(..),
#endif
#ifdef HTYPE_UID_T
                 CUid(..),
#endif
#ifdef HTYPE_UID_T
                 CUid(..),
#endif
                 Fd(..))
#endif

infixl 7 *, `times`
infixl 6 +, `plus`, -, `minus`
infixr 8 ^

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

-- | Raise a number to a non-negative integral power.
-- If the power is negative, this will call 'error'.
{-# SPECIALISE [1] (^) ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] (^) #-} -- See note [Inlining (^)]
(^) :: (Semiring a, Integral b) => a -> b -> a
x ^ y
  | y < 0 = error "Data.Semiring.^: negative power"
  | y == 0 = one
  | otherwise = getMul (stimes y (Mul x))

{- Note [Inlining (^)]
   ~~~~~~~~~~~~~~~~~~~
   The INLINABLE pragma allows (^) to be specialised at its call sites.
   If it is called repeatedly at the same type, that can make a huge
   difference, because of those constants which can be repeatedly
   calculated.

   Currently the fromInteger calls are not floated because we get
             \d1 d2 x y -> blah
   after the gentle round of simplification.
-}

{- Rules for powers with known small exponent
    see Trac #5237
    For small exponents, (^) is inefficient compared to manually
    expanding the multiplication tree.
    Here, rules for the most common exponent types are given.
    The range of exponents for which rules are given is quite
    arbitrary and kept small to not unduly increase the number of rules.
    It might be desirable to have corresponding rules also for
    exponents of other types (e.g., Word), but it's doubtful they
    would fire, since the exponents of other types tend to get
    floated out before the rule has a chance to fire. (Why?)

    Note: Trying to save multiplication by sharing the square for
    exponents 4 and 5 does not save time, indeed, for Double, it is
    up to twice slower, so the rules contain flat sequences of
    multiplications.
-}

{-# RULES
"^0/Int" forall x. x ^ (0 :: Int) = one
"^1/Int" forall x. x ^ (1 :: Int) = let u = x in u
"^2/Int" forall x. x ^ (2 :: Int) = let u = x in u*u
"^3/Int" forall x. x ^ (3 :: Int) = let u = x in u*u*u
"^4/Int" forall x. x ^ (4 :: Int) = let u = x in u*u*u*u
"^5/Int" forall x. x ^ (5 :: Int) = let u = x in u*u*u*u*u
"^0/Integer" forall x. x ^ (0 :: Integer) = one
"^1/Integer" forall x. x ^ (1 :: Integer) = let u = x in u
"^2/Integer" forall x. x ^ (2 :: Integer) = let u = x in u*u
"^3/Integer" forall x. x ^ (3 :: Integer) = let u = x in u*u*u
"^4/Integer" forall x. x ^ (4 :: Integer) = let u = x in u*u*u*u
"^5/Integer" forall x. x ^ (5 :: Integer) = let u = x in u*u*u*u*u
  #-}

-- | Infix shorthand for 'plus'.
(+) :: Semiring a => a -> a -> a
(+) = plus
{-# INLINE (+) #-}

-- | Infix shorthand for 'times'.
(*) :: Semiring a => a -> a -> a
(*) = times
{-# INLINE (*) #-}

-- | Infix shorthand for 'minus'.
(-) :: Ring a => a -> a -> a
(-) = minus
{-# INLINE (-) #-}

-- | Map each element of the structure to a semiring, and combine the results
--   using 'plus'.
foldMapP :: (Foldable t, Semiring s) => (a -> s) -> t a -> s
foldMapP f = Foldable.foldr (plus  . f) zero
{-# INLINE foldMapP #-}

-- | Map each element of the structure to a semiring, and combine the results
--   using 'times'.
foldMapT :: (Foldable t, Semiring s) => (a -> s) -> t a -> s
foldMapT f = Foldable.foldr (times . f) one
{-# INLINE foldMapT #-}

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce

-- | The 'sum' function computes the additive sum of the elements in a structure.
--   This function is lazy. For a strict version, see 'sum''.
sum :: (Foldable t, Semiring a) => t a -> a
sum = getAdd #. foldMap Add
{-# INLINE sum #-}

-- | The 'product' function computes the product of the elements in a structure.
--   This function is lazy. for a strict version, see 'product''.
product :: (Foldable t, Semiring a) => t a -> a
product = getMul #. foldMap Mul
{-# INLINE product #-}

-- | The 'sum'' function computes the additive sum of the elements in a structure.
--   This function is strict. For a lazy version, see 'sum'.
sum'  :: (Foldable t, Semiring a) => t a -> a
sum'  = Foldable.foldl' plus zero
{-# INLINE sum' #-}

-- | The 'product'' function computes the additive sum of the elements in a structure.
--   This function is strict. For a lazy version, see 'product'.
product' :: (Foldable t, Semiring a) => t a -> a
product' = Foldable.foldl' times one
{-# INLINE product' #-}

-- | Monoid under 'plus'. Analogous to 'Data.Monoid.Sum', but
--   uses the 'Semiring' constraint rather than 'Num.Num'.
newtype Add a = Add { getAdd :: a }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Foldable
    , Fractional
    , Functor
    , Generic
    , Generic1
    , Num.Num
    , Ord
    , Read
    , Real
    , RealFrac
    , Show
    , Storable
    , Traversable
    , Typeable
    )

instance Semiring a => Semigroup (Add a) where
  Add a <> Add b = Add (a + b)
  stimes n (Add a) = Add (fromNatural (Real.fromIntegral n) * a)
  {-# INLINE (<>) #-}

instance Semiring a => Monoid (Add a) where
  mempty = Add zero
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

-- | This is an internal type, solely for purposes
-- of default implementation of 'fromNatural'.
newtype Add' a = Add' { getAdd' :: a }

instance Semiring a => Semigroup (Add' a) where
  Add' a <> Add' b = Add' (a + b)

-- | Monoid under 'times'. Analogous to 'Data.Monoid.Product', but
--   uses the 'Semiring' constraint rather than 'Num.Num'.
newtype Mul a = Mul { getMul :: a }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Foldable
    , Fractional
    , Functor
    , Generic
    , Generic1
    , Num.Num
    , Ord
    , Read
    , Real
    , RealFrac
    , Show
    , Storable
    , Traversable
    , Typeable
    )

instance Semiring a => Semigroup (Mul a) where
  Mul a <> Mul b = Mul (a * b)
  {-# INLINE (<>) #-}
  stimes n x0 = case compare n 0 of
    LT -> error "stimes: negative multiplier"
    EQ -> mempty
    GT -> f x0 n
      where
        f x y
          | even y = f (x `mappend` x) (y `quot` 2)
          | y == 1 = x
          | otherwise = g (x `mappend` x) (y `quot` 2) x
        g x y z
          | even y = g (x `mappend` x) (y `quot` 2) z
          | y == 1 = x `mappend` z
          | otherwise = g (x `mappend` x) (y `quot` 2) (x `mappend` z)
  {-# INLINE stimes #-}

instance Semiring a => Monoid (Mul a) where
  mempty = Mul one
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

-- | Provide Semiring and Ring for an arbitrary 'Num.Num'. It is useful with GHC 8.6+'s DerivingVia extension.
newtype WrappedNum a = WrapNum { unwrapNum :: a }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Foldable
    , Fractional
    , Functor
    , Generic
    , Generic1
    , Num.Num
    , Ord
    , Read
    , Real
    , RealFrac
    , Show
    , Storable
    , Traversable
    , Typeable
    , Bits
    )

instance Num.Num a => Semiring (WrappedNum a) where
  plus  = (Num.+)
  zero  = 0
  times = (Num.*)
  one   = 1
  fromNatural = Real.fromIntegral

instance Num.Num a => Ring (WrappedNum a) where
  negate = Num.negate

-- | 'Mod2' represents the integers mod 2.
--
--   It is useful in the computing of <https://en.wikipedia.org/wiki/Zhegalkin_polynomial Zhegalkin polynomials>.
newtype Mod2 = Mod2 { getMod2 :: Bool }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Read
    , Show
    , Generic
    )

instance Semiring Mod2 where
  plus (Mod2 x) (Mod2 y) = Mod2 (x `xor` y)
  times (Mod2 x) (Mod2 y) = Mod2 (x && y)
  zero = Mod2 False
  one = Mod2 True

instance Ring Mod2 where
  negate = id
  {-# INLINE negate #-}


{--------------------------------------------------------------------
  Classes
--------------------------------------------------------------------}

-- | The class of semirings (types with two binary
-- operations and two respective identities). One
-- can think of a semiring as two monoids of the same
-- underlying type, with the first being commutative.
-- In the documentation, you will often see the first
-- monoid being referred to as @additive@, and the second
-- monoid being referred to as @multiplicative@, a typical
-- convention when talking about semirings.
--
-- For any type R with a 'Num.Num'
-- instance, the additive monoid is (R, 'Prelude.+', 0)
-- and the multiplicative monoid is (R, 'Prelude.*', 1).
--
-- For 'Prelude.Bool', the additive monoid is ('Prelude.Bool', 'Prelude.||', 'Prelude.False')
-- and the multiplicative monoid is ('Prelude.Bool', 'Prelude.&&', 'Prelude.True').
--
-- Instances should satisfy the following laws:
--
-- [/additive left identity/]
--     @'zero' '+' x = x@
-- [/additive right identity/]
--     @x '+' 'zero' = x@
-- [/additive associativity/]
--     @x '+' (y '+' z) = (x '+' y) '+' z@
-- [/additive commutativity/]
--     @x '+' y = y '+' x@
-- [/multiplicative left identity/]
--     @'one' '*' x = x@
-- [/multiplicative right identity/]
--     @x '*' 'one' = x@
-- [/multiplicative associativity/]
--     @x '*' (y '*' z) = (x '*' y) '*' z@
-- [/left-distributivity of '*' over '+'/]
--     @x '*' (y '+' z) = (x '*' y) '+' (x '*' z)@
-- [/right-distributivity of '*' over '+'/]
--     @(x '+' y) '*' z = (x '*' z) '+' (y '*' z)@
-- [/annihilation/]
--     @'zero' '*' x = x '*' 'zero' = 'zero'@

class Semiring a where
  {-# MINIMAL plus, times, (zero, one | fromNatural) #-}
  plus  :: a -> a -> a -- ^ Commutative Operation
  zero  :: a           -- ^ Commutative Unit
  zero = fromNatural 0
  times :: a -> a -> a -- ^ Associative Operation
  one   :: a           -- ^ Associative Unit
  one = fromNatural 1
  fromNatural :: Natural -> a -- ^ Homomorphism of additive semigroups
  fromNatural 0 = zero
  fromNatural n = getAdd' (stimes n (Add' one))

-- | The class of semirings with an additive inverse.
--
--     @'negate' a '+' a = 'zero'@

class Semiring a => Ring a where
  {-# MINIMAL negate #-}
  negate :: a -> a

-- | Subtract two 'Ring' values. For any type @R@ with
-- a 'Num.Num' instance, this is the same as '(Prelude.-)'.
--
--     @x `minus` y = x '+' 'negate' y@
minus :: Ring a => a -> a -> a
minus x y = x + negate y
{-# INLINE minus #-}

-- | Convert from integer to ring.
--
-- When @{-#@ @LANGUAGE RebindableSyntax #-}@ is enabled,
-- this function is used for desugaring integer literals.
-- This may be used to facilitate transition from 'Num.Num' to 'Ring':
-- no need to replace 0 and 1 with 'one' and 'zero'
-- or to cast numeric literals.
fromInteger :: Ring a => Integer -> a
fromInteger x
  | x >= 0    = fromNatural (Num.fromInteger x)
  | otherwise = negate (fromNatural (Num.fromInteger (Num.negate x)))
{-# INLINE fromInteger #-}

-- | Convert from integral to ring.
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral x
  | x >= 0    = fromNatural (Real.fromIntegral x)
  | otherwise = negate (fromNatural (Real.fromIntegral (Num.negate x)))
{-# INLINE fromIntegral #-}

{--------------------------------------------------------------------
  Instances (base)
--------------------------------------------------------------------}

instance (Semiring a, Semiring b) => Semiring (a,b) where
  zero = (zero, zero)
  one = (one, one)
  plus (x1, x2) (y1, y2) =
    (x1 `plus` y1, x2 `plus` y2)
  times (x1, x2) (y1, y2) =
    (x1 `times` y1, x2 `times` y2)

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
  zero = (zero, zero, zero)
  one = (one, one, one)
  plus (x1, x2, x3) (y1, y2, y3) =
    (x1 `plus` y1, x2 `plus` y2, x3 `plus` y3)
  times (x1, x2, x3) (y1, y2, y3) =
    (x1 `times` y1, x2 `times` y2, x3 `times` y3)

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
  zero = (zero, zero, zero, zero)
  one = (one, one, one, one)
  plus (x1, x2, x3, x4) (y1, y2, y3, y4) =
    (x1 `plus` y1, x2 `plus` y2, x3 `plus` y3, x4 `plus` y4)
  times (x1, x2, x3, x4) (y1, y2, y3, y4) =
    (x1 `times` y1, x2 `times` y2, x3 `times` y3, x4 `times` y4)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a,b,c,d,e) where
  zero = (zero, zero, zero, zero, zero)
  one = (one, one, one, one, one)
  plus (x1, x2, x3, x4, x5) (y1, y2, y3, y4, y5) =
    (x1 `plus` y1, x2 `plus` y2, x3 `plus` y3, x4 `plus` y4, x5 `plus` y5)
  times (x1, x2, x3, x4, x5) (y1, y2, y3, y4, y5) =
    (x1 `times` y1, x2 `times` y2, x3 `times` y3, x4 `times` y4, x5 `times` y5)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f) => Semiring (a,b,c,d,e,f) where
  zero = (zero, zero, zero, zero, zero, zero)
  one = (one, one, one, one, one, one)
  plus (x1, x2, x3, x4, x5, x6) (y1, y2, y3, y4, y5, y6) =
    (x1 `plus` y1, x2 `plus` y2, x3 `plus` y3, x4 `plus` y4, x5 `plus` y5, x6 `plus` y6)
  times (x1, x2, x3, x4, x5, x6) (y1, y2, y3, y4, y5, y6) =
    (x1 `times` y1, x2 `times` y2, x3 `times` y3, x4 `times` y4, x5 `times` y5, x6 `times` y6)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g) => Semiring (a,b,c,d,e,f,g) where
  zero = (zero, zero, zero, zero, zero, zero, zero)
  one = (one, one, one, one, one, one, one)
  plus (x1, x2, x3, x4, x5, x6, x7) (y1, y2, y3, y4, y5, y6, y7) =
    (x1 `plus` y1, x2 `plus` y2, x3 `plus` y3, x4 `plus` y4, x5 `plus` y5, x6 `plus` y6, x7 `plus` y7)
  times (x1, x2, x3, x4, x5, x6, x7) (y1, y2, y3, y4, y5, y6, y7) =
    (x1 `times` y1, x2 `times` y2, x3 `times` y3, x4 `times` y4, x5 `times` y5, x6 `times` y6, x7 `times` y7)

instance (Ring a, Ring b) => Ring (a,b) where
  negate (x1, x2) = (negate x1, negate x2)

instance (Ring a, Ring b, Ring c) => Ring (a,b,c) where
  negate (x1, x2, x3) = (negate x1, negate x2, negate x3)

instance (Ring a, Ring b, Ring c, Ring d) => Ring (a,b,c,d) where
  negate (x1, x2, x3, x4) = (negate x1, negate x2, negate x3, negate x4)

instance (Ring a, Ring b, Ring c, Ring d, Ring e) => Ring (a,b,c,d,e) where
  negate (x1, x2, x3, x4, x5) = (negate x1, negate x2, negate x3, negate x4, negate x5)

instance (Ring a, Ring b, Ring c, Ring d, Ring e, Ring f) => Ring (a,b,c,d,e,f) where
  negate (x1, x2, x3, x4, x5, x6) = (negate x1, negate x2, negate x3, negate x4, negate x5, negate x6)

instance (Ring a, Ring b, Ring c, Ring d, Ring e, Ring f, Ring g) => Ring (a,b,c,d,e,f,g) where
  negate (x1, x2, x3, x4, x5, x6, x7) = (negate x1, negate x2, negate x3, negate x4, negate x5, negate x6, negate x7)

instance Semiring b => Semiring (a -> b) where
  plus f g    = \x -> f x `plus` g x
  zero        = const zero
  times f g   = \x -> f x `times` g x
  one         = const one
  fromNatural = const . fromNatural
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Ring b => Ring (a -> b) where
  negate f x = negate (f x)
  {-# INLINE negate #-}

instance Semiring () where
  plus _ _  = ()
  zero      = ()
  times _ _ = ()
  one       = ()
  fromNatural _ = ()
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Ring () where
  negate _ = ()
  {-# INLINE negate #-}

instance Semiring (Proxy a) where
  plus _ _  = Proxy
  zero      = Proxy
  times _ _ = Proxy
  one       = Proxy
  fromNatural _ = Proxy
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Semiring Bool where
  plus  = (||)
  zero  = False
  times = (&&)
  one   = True
  fromNatural 0 = False
  fromNatural _ = True
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Semiring a => Semiring (Maybe a) where
  zero  = Nothing
  one   = Just one

  plus Nothing y = y
  plus x Nothing = x
  plus (Just x) (Just y) = Just (plus x y)

  times Nothing _ = Nothing
  times _ Nothing = Nothing
  times (Just x) (Just y) = Just (times x y)

  fromNatural 0 = Nothing
  fromNatural n = Just (fromNatural n)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Semiring a => Semiring (IO a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times
  fromNatural = pure . fromNatural
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Ring a => Ring (IO a) where
  negate = fmap negate
  {-# INLINE negate #-}

instance Semiring a => Semiring (Dual a) where
  zero = Dual zero
  Dual x `plus` Dual y = Dual (y `plus` x)
  one = Dual one
  Dual x `times` Dual y = Dual (y `times` x)
  fromNatural = Dual . fromNatural
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Ring a => Ring (Dual a) where
  negate (Dual x) = Dual (negate x)
  {-# INLINE negate #-}

instance Semiring a => Semiring (Const a b) where
  zero = Const zero
  one  = Const one
  plus  (Const x) (Const y) = Const (x `plus`  y)
  times (Const x) (Const y) = Const (x `times` y)
  fromNatural = Const . fromNatural
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Ring a => Ring (Const a b) where
  negate (Const x) = Const (negate x)
  {-# INLINE negate #-}

-- | This instance can suffer due to floating point arithmetic.
instance Ring a => Semiring (Complex a) where
  zero = zero :+ zero
  one  = one  :+ zero
  plus  (x :+ y) (x' :+ y') = plus x x' :+ plus y y'
  times (x :+ y) (x' :+ y')
    = (x * x' - (y * y')) :+ (x * y' + y * x')
  fromNatural n = fromNatural n :+ zero
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance Ring a => Ring (Complex a) where
  negate (x :+ y) = negate x :+ negate y
  {-# INLINE negate #-}

#if MIN_VERSION_base(4,12,0)
instance (Semiring a, Applicative f) => Semiring (Ap f a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times
  fromNatural = pure . fromNatural
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

instance (Ring a, Applicative f) => Ring (Ap f a) where
  negate = fmap negate
  {-# INLINE negate #-}
#endif

#if MIN_VERSION_base(4,12,0)
deriving instance Semiring (Predicate a)

deriving instance Semiring a => Semiring (Equivalence a)

deriving instance Semiring a => Semiring (Op a b)
deriving instance Ring a => Ring (Op a b)
#endif

instance Integral a => Semiring (Ratio a) where
  {-# SPECIALIZE instance Semiring Rational #-}
  zero  = 0 % 1
  one   = 1 % 1
  plus  = (Num.+)
  times = (Num.*)
  fromNatural n = Real.fromIntegral n % 1
  {-# INLINE zero  #-}
  {-# INLINE one   #-}
  {-# INLINE plus  #-}
  {-# INLINE times #-}
  {-# INLINE fromNatural #-}
deriving instance Semiring a => Semiring (Identity a)
deriving instance Semiring a => Semiring (Down a)
instance HasResolution a => Semiring (Fixed a) where
  zero  = 0
  one   = 1
  plus  = (Num.+)
  times = (Num.*)
  fromNatural = Real.fromIntegral
  {-# INLINE zero  #-}
  {-# INLINE one   #-}
  {-# INLINE plus  #-}
  {-# INLINE times #-}
  {-# INLINE fromNatural #-}

instance Integral a => Ring (Ratio a) where
  negate = Num.negate
  {-# INLINE negate #-}

deriving instance Ring a => Ring (Down a)
deriving instance Ring a => Ring (Identity a)
instance HasResolution a => Ring (Fixed a) where
  negate = Num.negate
  {-# INLINE negate #-}

{--------------------------------------------------------------------
  Instances (containers)
--------------------------------------------------------------------}

#if defined(VERSION_containers)

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid', so we require a
--   'Monoid' constraint instead of a 'Semiring'
--   constraint since 'times' can use
--   the context of either.
instance (Ord a, Monoid a) => Semiring (Set a) where
  zero  = Set.empty
  one   = Set.singleton mempty
  plus  = Set.union
  times xs ys = Foldable.foldMap (flip Set.map ys . mappend) xs
  fromNatural 0 = zero
  fromNatural _ = one
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

-- | Wrapper to mimic 'Set' ('Data.Semigroup.Sum' 'Int'),
-- 'Set' ('Data.Semigroup.Product' 'Int'), etc.,
-- while having a more efficient underlying representation.
newtype IntSetOf a = IntSetOf { getIntSet :: IntSet }
  deriving
    ( Eq
    , Generic
    , Generic1
    , Ord
    , Read
    , Show
    , Typeable
    , Semigroup
    , Monoid
    )

instance (Coercible Int a, Monoid a) => Semiring (IntSetOf a) where
  zero  = coerce IntSet.empty
  one   = coerce IntSet.singleton (mempty :: a)
  plus  = coerce IntSet.union
  xs `times` ys
    = coerce IntSet.fromList
        [ mappend k l
        | k :: a <- coerce IntSet.toList xs
        , l :: a <- coerce IntSet.toList ys
        ]
  fromNatural 0 = zero
  fromNatural _ = one
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid' as the key type,
--   so we require a 'Monoid' constraint instead of
--   a 'Semiring' constraint since 'times' can use
--   the context of either.
instance (Ord k, Monoid k, Semiring v) => Semiring (Map k v) where
  zero = Map.empty
  one  = Map.singleton mempty one
  plus = Map.unionWith (+)
  xs `times` ys
    = Map.fromListWith (+)
        [ (mappend k l, v * u)
        | (k,v) <- Map.toList xs
        , (l,u) <- Map.toList ys
        ]
  fromNatural 0 = zero
  fromNatural n = Map.singleton mempty (fromNatural n)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

-- | Wrapper to mimic 'Map' ('Data.Semigroup.Sum' 'Int') v,
-- 'Map' ('Data.Semigroup.Product' 'Int') v, etc.,
-- while having a more efficient underlying representation.
newtype IntMapOf k v = IntMapOf { getIntMap :: IntMap v }
  deriving
    ( Eq
    , Generic
    , Generic1
    , Ord
    , Read
    , Show
    , Typeable
    , Semigroup
    , Monoid
    )

instance (Coercible Int k, Monoid k, Semiring v) => Semiring (IntMapOf k v) where
  zero = coerce (IntMap.empty :: IntMap v)
  one  = coerce (IntMap.singleton :: Int -> v -> IntMap v) (mempty :: k) (one :: v)
  plus = coerce (IntMap.unionWith (+) :: IntMap v -> IntMap v -> IntMap v)
  xs `times` ys
    = coerce (IntMap.fromListWith (+) :: [(Int, v)] -> IntMap v)
        [ (mappend k l, v * u)
        | (k :: k, v :: v) <- coerce (IntMap.toList :: IntMap v -> [(Int, v)]) xs
        , (l :: k, u :: v) <- coerce (IntMap.toList :: IntMap v -> [(Int, v)]) ys
        ]
  fromNatural 0 = zero
  fromNatural n = coerce (IntMap.singleton :: Int -> v -> IntMap v) (mempty :: k) (fromNatural n :: v)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

#endif

{--------------------------------------------------------------------
  Instances (unordered-containers)
--------------------------------------------------------------------}

#if defined(VERSION_unordered_containers)

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid', so we require a
--   'Monoid' constraint instead of a 'Semiring'
--   constraint since 'times' can use
--   the context of either.
instance (Eq a, Hashable a, Monoid a) => Semiring (HashSet a) where
  zero = HashSet.empty
  one  = HashSet.singleton mempty
  plus = HashSet.union
  times xs ys = Foldable.foldMap (flip HashSet.map ys . mappend) xs
  fromNatural 0 = zero
  fromNatural _ = one
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid' as the key type,
--   so we require a 'Monoid' constraint instead of
--   a 'Semiring' constraint since 'times' can use
--   the context of either.
instance (Eq k, Hashable k, Monoid k, Semiring v) => Semiring (HashMap k v) where
  zero = HashMap.empty
  one  = HashMap.singleton mempty one
  plus = HashMap.unionWith (+)
  xs `times` ys
    = HashMap.fromListWith (+)
        [ (mappend k l, v * u)
        | (k,v) <- HashMap.toList xs
        , (l,u) <- HashMap.toList ys
        ]
  fromNatural 0 = zero
  fromNatural n = HashMap.singleton mempty (fromNatural n)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  {-# INLINE fromNatural #-}
#endif

-- | Is the value 'zero'?
isZero :: (Eq a, Semiring a) => a -> Bool
isZero x = x == zero
{-# INLINEABLE isZero #-}

-- | Is the value 'one'?
isOne :: (Eq a, Semiring a) => a -> Bool
isOne x = x == one
{-# INLINEABLE isOne #-}

#if MIN_VERSION_base(4,12,0)
deriving via (WrappedNum Int) instance Semiring Int
deriving via (WrappedNum Int8) instance Semiring Int8
deriving via (WrappedNum Int16) instance Semiring Int16
deriving via (WrappedNum Int32) instance Semiring Int32
deriving via (WrappedNum Int64) instance Semiring Int64
deriving via (WrappedNum Integer) instance Semiring Integer
deriving via (WrappedNum Word) instance Semiring Word
deriving via (WrappedNum Word8) instance Semiring Word8
deriving via (WrappedNum Word16) instance Semiring Word16
deriving via (WrappedNum Word32) instance Semiring Word32
deriving via (WrappedNum Word64) instance Semiring Word64
deriving via (WrappedNum Float) instance Semiring Float
deriving via (WrappedNum Double) instance Semiring Double
deriving via (WrappedNum CUIntMax) instance Semiring CUIntMax
deriving via (WrappedNum CIntMax) instance Semiring CIntMax
deriving via (WrappedNum CUIntPtr) instance Semiring CUIntPtr
deriving via (WrappedNum CIntPtr) instance Semiring CIntPtr
deriving via (WrappedNum CSUSeconds) instance Semiring CSUSeconds
deriving via (WrappedNum CUSeconds) instance Semiring CUSeconds
deriving via (WrappedNum CTime) instance Semiring CTime
deriving via (WrappedNum CClock) instance Semiring CClock
deriving via (WrappedNum CSigAtomic) instance Semiring CSigAtomic
deriving via (WrappedNum CWchar) instance Semiring CWchar
deriving via (WrappedNum CSize) instance Semiring CSize
deriving via (WrappedNum CPtrdiff) instance Semiring CPtrdiff
deriving via (WrappedNum CDouble) instance Semiring CDouble
deriving via (WrappedNum CFloat) instance Semiring CFloat
deriving via (WrappedNum CULLong) instance Semiring CULLong
deriving via (WrappedNum CLLong) instance Semiring CLLong
deriving via (WrappedNum CULong) instance Semiring CULong
deriving via (WrappedNum CLong) instance Semiring CLong
deriving via (WrappedNum CUInt) instance Semiring CUInt
deriving via (WrappedNum CInt) instance Semiring CInt
deriving via (WrappedNum CUShort) instance Semiring CUShort
deriving via (WrappedNum CShort) instance Semiring CShort
deriving via (WrappedNum CUChar) instance Semiring CUChar
deriving via (WrappedNum CSChar) instance Semiring CSChar
deriving via (WrappedNum CChar) instance Semiring CChar
deriving via (WrappedNum IntPtr) instance Semiring IntPtr
deriving via (WrappedNum WordPtr) instance Semiring WordPtr
#if !HOST_OS_WINDOWS
#ifdef HTYPE_CC_T
deriving via (WrappedNum CCc) instance Semiring CCc
#endif
#ifdef HTYPE_DEV_T
deriving via (WrappedNum CDev) instance Semiring CDev
#endif
#ifdef HTYPE_GID_T
deriving via (WrappedNum CGid) instance Semiring CGid
#endif
#ifdef HTYPE_INO_T
deriving via (WrappedNum CIno) instance Semiring CIno
#endif
#ifdef HTYPE_MODE_T
deriving via (WrappedNum CMode) instance Semiring CMode
#endif
#ifdef HTYPE_NLINK_T
deriving via (WrappedNum CNlink) instance Semiring CNlink
#endif
#ifdef HTYPE_OFF_T
deriving via (WrappedNum COff) instance Semiring COff
#endif
#ifdef HTYPE_PID_T
deriving via (WrappedNum CPid) instance Semiring CPid
#endif
#ifdef HTYPE_RLIM_T
deriving via (WrappedNum CRLim) instance Semiring CRLim
#endif
#ifdef HTYPE_SPEED_T
deriving via (WrappedNum CSpeed) instance Semiring CSpeed
#endif
#ifdef HTYPE_SSIZE_T
deriving via (WrappedNum CSsize) instance Semiring CSsize
#endif
#ifdef HTYPE_TCFLAG_T
deriving via (WrappedNum CTcflag) instance Semiring CTcflag
#endif
#ifdef HTYPE_UID_T
deriving via (WrappedNum CUid) instance Semiring CUid
#endif
deriving via (WrappedNum Fd) instance Semiring Fd
#endif
deriving via (WrappedNum Natural) instance Semiring Natural
#else
-- Integral and fieldlike instances
$(let
  deriveSemiring :: Q Type -> Q [Dec]
  deriveSemiring ty = [d|
      instance Semiring $ty where
         zero  = 0
         one   = 1
         plus  x y = (Num.+) x y
         times x y = (Num.*) x y
         fromNatural = Real.fromIntegral
         {-# INLINE zero #-}
         {-# INLINE one  #-}
         {-# INLINE plus #-}
         {-# INLINE times #-}
         {-# INLINE fromNatural #-}
      |]

  in P.concat P.<$> P.traverse deriveSemiring
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
   ,[t|Float|]
   ,[t|Double|]
   ,[t|CUIntMax|]
   ,[t|CIntMax|]
   ,[t|CUIntPtr|]
   ,[t|CIntPtr|]
   ,[t|CSUSeconds|]
   ,[t|CUSeconds|]
   ,[t|CTime|]
   ,[t|CClock|]
   ,[t|CSigAtomic|]
   ,[t|CWchar|]
   ,[t|CSize|]
   ,[t|CPtrdiff|]
   ,[t|CDouble|]
   ,[t|CFloat|]
   ,[t|CULLong|]
   ,[t|CLLong|]
   ,[t|CULong|]
   ,[t|CLong|]
   ,[t|CUInt|]
   ,[t|CInt|]
   ,[t|CUShort|]
   ,[t|CShort|]
   ,[t|CUChar|]
   ,[t|CSChar|]
   ,[t|CChar|]
   ,[t|IntPtr|]
   ,[t|WordPtr|]
#if !HOST_OS_WINDOWS
   ,[t|CCc|]
   ,[t|CDev|]
   ,[t|CGid|]
   ,[t|CIno|]
   ,[t|CMode|]
   ,[t|CNlink|]
   ,[t|COff|]
   ,[t|CPid|]
   ,[t|CRLim|]
   ,[t|CSpeed|]
   ,[t|CSsize|]
   ,[t|CTcflag|]
   ,[t|CUid|]
   ,[t|Fd|]
#endif
   ,[t|Natural|]
   ])
#endif

#if MIN_VERSION_base(4,12,0)
deriving via (WrappedNum Int) instance Ring Int
deriving via (WrappedNum Int8) instance Ring Int8
deriving via (WrappedNum Int16) instance Ring Int16
deriving via (WrappedNum Int32) instance Ring Int32
deriving via (WrappedNum Int64) instance Ring Int64
deriving via (WrappedNum Integer) instance Ring Integer
deriving via (WrappedNum Word) instance Ring Word
deriving via (WrappedNum Word8) instance Ring Word8
deriving via (WrappedNum Word16) instance Ring Word16
deriving via (WrappedNum Word32) instance Ring Word32
deriving via (WrappedNum Word64) instance Ring Word64
deriving via (WrappedNum Float) instance Ring Float
deriving via (WrappedNum Double) instance Ring Double
deriving via (WrappedNum CUIntMax) instance Ring CUIntMax
deriving via (WrappedNum CIntMax) instance Ring CIntMax
deriving via (WrappedNum CUIntPtr) instance Ring CUIntPtr
deriving via (WrappedNum CIntPtr) instance Ring CIntPtr
deriving via (WrappedNum CSUSeconds) instance Ring CSUSeconds
deriving via (WrappedNum CUSeconds) instance Ring CUSeconds
deriving via (WrappedNum CTime) instance Ring CTime
deriving via (WrappedNum CClock) instance Ring CClock
deriving via (WrappedNum CSigAtomic) instance Ring CSigAtomic
deriving via (WrappedNum CWchar) instance Ring CWchar
deriving via (WrappedNum CSize) instance Ring CSize
deriving via (WrappedNum CPtrdiff) instance Ring CPtrdiff
deriving via (WrappedNum CDouble) instance Ring CDouble
deriving via (WrappedNum CFloat) instance Ring CFloat
deriving via (WrappedNum CULLong) instance Ring CULLong
deriving via (WrappedNum CLLong) instance Ring CLLong
deriving via (WrappedNum CULong) instance Ring CULong
deriving via (WrappedNum CLong) instance Ring CLong
deriving via (WrappedNum CUInt) instance Ring CUInt
deriving via (WrappedNum CInt) instance Ring CInt
deriving via (WrappedNum CUShort) instance Ring CUShort
deriving via (WrappedNum CShort) instance Ring CShort
deriving via (WrappedNum CUChar) instance Ring CUChar
deriving via (WrappedNum CSChar) instance Ring CSChar
deriving via (WrappedNum CChar) instance Ring CChar
deriving via (WrappedNum IntPtr) instance Ring IntPtr
deriving via (WrappedNum WordPtr) instance Ring WordPtr

#if !HOST_OS_WINDOWS
#ifdef HTYPE_CC_T
deriving via (WrappedNum CCc) instance Ring CCc
#endif
#ifdef HTYPE_DEV_T
deriving via (WrappedNum CDev) instance Ring CDev
#endif
#ifdef HTYPE_GID_T
deriving via (WrappedNum CGid) instance Ring CGid
#endif
#ifdef HTYPE_INO_T
deriving via (WrappedNum CIno) instance Ring CIno
#endif
#ifdef HTYPE_MODE_T
deriving via (WrappedNum CMode) instance Ring CMode
#endif
#ifdef HTYPE_NLINK_T
deriving via (WrappedNum CNlink) instance Ring CNlink
#endif
#ifdef HTYPE_OFF_T
deriving via (WrappedNum COff) instance Ring COff
#endif
#ifdef HTYPE_PID_T
deriving via (WrappedNum CPid) instance Ring CPid
#endif
#ifdef HTYPE_RLIM_T
deriving via (WrappedNum CRLim) instance Ring CRLim
#endif
#ifdef HTYPE_SPEED_T
deriving via (WrappedNum CSpeed) instance Ring CSpeed
#endif
#ifdef HTYPE_SSIZE_T
deriving via (WrappedNum CSsize) instance Ring CSsize
#endif
#ifdef HTYPE_TCFLAG_T
deriving via (WrappedNum CTcflag) instance Ring CTcflag
#endif
#ifdef HTYPE_UID_T
deriving via (WrappedNum CUid) instance Ring CUid
#endif
deriving via (WrappedNum Fd) instance Ring Fd
#endif
#else
$(let
  deriveRing :: Q Type -> Q [Dec]
  deriveRing ty = [d|
      instance Ring $ty where
        negate = Num.negate
        {-# INLINE negate #-}
      |]

  in P.concat P.<$> P.traverse deriveRing
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
    ,[t|Float|]
    ,[t|Double|]
    ,[t|CUIntMax|]
    ,[t|CIntMax|]
    ,[t|CUIntPtr|]
    ,[t|CIntPtr|]
    ,[t|CSUSeconds|]
    ,[t|CUSeconds|]
    ,[t|CTime|]
    ,[t|CClock|]
    ,[t|CSigAtomic|]
    ,[t|CWchar|]
    ,[t|CSize|]
    ,[t|CPtrdiff|]
    ,[t|CDouble|]
    ,[t|CFloat|]
    ,[t|CULLong|]
    ,[t|CLLong|]
    ,[t|CULong|]
    ,[t|CLong|]
    ,[t|CUInt|]
    ,[t|CInt|]
    ,[t|CUShort|]
    ,[t|CShort|]
    ,[t|CUChar|]
    ,[t|CSChar|]
    ,[t|CChar|]
    ,[t|IntPtr|]
    ,[t|WordPtr|]

#if !HOST_OS_WINDOWS
    ,[t|CCc|]
    ,[t|CDev|]
    ,[t|CGid|]
    ,[t|CIno|]
    ,[t|CMode|]
    ,[t|CNlink|]
    ,[t|COff|]
    ,[t|CPid|]
    ,[t|CRLim|]
    ,[t|CSpeed|]
    ,[t|CSsize|]
    ,[t|CTcflag|]
    ,[t|CUid|]
    ,[t|Fd|]
#endif
    ])
#endif
