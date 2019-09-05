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
#if MIN_VERSION_base(4,7,0) && !MIN_VERSION_base(4,8,0)
{-# LANGUAGE UndecidableInstances       #-} -- on GHC 7.8 the coercible constraint causes us to need this
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
#if defined(VERSION_containers) && MIN_VERSION_base(4,7,0)
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
import           Data.Bits (Bits)
import           Data.Bool (Bool(..), (||), (&&), otherwise)
#if MIN_VERSION_base(4,7,0)
import           Data.Coerce (Coercible, coerce)
#endif
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
#if MIN_VERSION_base(4,7,0)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
#endif
import           Data.Map (Map)
import qualified Data.Map as Map
#endif
import           Data.Monoid (Monoid(..), Dual(..))
import           Data.Ord (Ord((<)), (>=))
#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down(..))
#endif
import           Data.Proxy (Proxy(..))
import           Data.Ratio (Ratio, Rational, (%))
import           Data.Semigroup (Semigroup(..))
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
#if MIN_VERSION_base(4,6,1)
import           GHC.Generics (Generic,Generic1)
#endif
import           GHC.IO (IO)
import           GHC.Integer (Integer)
import qualified GHC.Num as Num
import           GHC.Read (Read)
import           GHC.Real (Integral, Fractional, Real, RealFrac)
import qualified GHC.Real as Real
import           GHC.Show (Show)
import           Numeric.Natural (Natural)

#ifdef mingw32_HOST_OS
#define HOST_OS_WINDOWS 1
#else
#define HOST_OS_WINDOWS 0
#endif

#if !HOST_OS_WINDOWS
import           System.Posix.Types
  (CCc, CDev, CGid, CIno, CMode, CNlink,
   COff, CPid, CRLim, CSpeed, CSsize,
   CTcflag, CUid, Fd)
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

#if MIN_VERSION_base(4,7,0)
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

#else

-- | The 'sum' function computes the additive sum of the elements in a structure.
--   This function is lazy. For a strict version, see 'sum''.
sum :: (Foldable t, Semiring a) => t a -> a
sum = getAdd . foldMap Add
{-# INLINE sum #-}

-- | The 'product' function computes the product of the elements in a structure.
--   This function is lazy. for a strict version, see 'product''.
product :: (Foldable t, Semiring a) => t a -> a
product = getMul . foldMap Mul
{-# INLINE product #-}

#endif

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
--   uses the 'Semiring' constraint rather than 'Num'.
newtype Add a = Add { getAdd :: a }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Foldable
    , Fractional
    , Functor
#if MIN_VERSION_base(4,6,1)
    , Generic
    , Generic1
#endif
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
--   uses the 'Semiring' constraint rather than 'Num'.
newtype Mul a = Mul { getMul :: a }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Foldable
    , Fractional
    , Functor
#if MIN_VERSION_base(4,6,1)
    , Generic
    , Generic1
#endif
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

instance Semiring a => Monoid (Mul a) where
  mempty = Mul one
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

-- | Provide Semiring and Ring for an arbitrary Num. It is useful with GHC 8.6+'s DerivingVia extension.
newtype WrappedNum a = WrapNum { unwrapNum :: a }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Foldable
    , Fractional
    , Functor
#if MIN_VERSION_base(4,6,1)
    , Generic
    , Generic1
#endif
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
#if MIN_VERSION_base(4,6,1)
    , Generic
#endif
    )

instance Semiring Mod2 where
  -- we inline the definition of 'xor'
  -- on Bools, since the instance did not exist until
  -- base-4.7.0.
  plus (Mod2 x) (Mod2 y) = Mod2 (x /= y)
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
-- monoid being referred to as 'additive', and the second
-- monoid being referred to as 'multiplicative', a typical
-- convention when talking about semirings.
--
-- For any type R with a 'Prelude.Num'
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
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL plus, times, (zero, one | fromNatural) #-}
#endif
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
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL negate #-}
#endif
  negate :: a -> a

-- | Subtract two 'Ring' values. For any type 'R' with
-- a 'Prelude.Num' instance, this is the same as '(Prelude.-)'.
--
--     @x `minus` y = x '+' 'negate' y@
minus :: Ring a => a -> a -> a
minus x y = x + negate y
{-# INLINE minus #-}

-- | Convert from integer to ring.
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

#define deriveSemiring(ty)         \
instance Semiring (ty) where {     \
   zero  = 0                       \
;  one   = 1                       \
;  plus  x y = (Num.+) x y         \
;  times x y = (Num.*) x y         \
;  fromNatural = Real.fromIntegral \
;  {-# INLINE zero #-}             \
;  {-# INLINE one  #-}             \
;  {-# INLINE plus #-}             \
;  {-# INLINE times #-}            \
;  {-# INLINE fromNatural #-}      \
}

deriveSemiring(Int)
deriveSemiring(Int8)
deriveSemiring(Int16)
deriveSemiring(Int32)
deriveSemiring(Int64)
deriveSemiring(Integer)
deriveSemiring(Word)
deriveSemiring(Word8)
deriveSemiring(Word16)
deriveSemiring(Word32)
deriveSemiring(Word64)
deriveSemiring(Float)
deriveSemiring(Double)
deriveSemiring(CUIntMax)
deriveSemiring(CIntMax)
deriveSemiring(CUIntPtr)
deriveSemiring(CIntPtr)
deriveSemiring(CSUSeconds)
deriveSemiring(CUSeconds)
deriveSemiring(CTime)
deriveSemiring(CClock)
deriveSemiring(CSigAtomic)
deriveSemiring(CWchar)
deriveSemiring(CSize)
deriveSemiring(CPtrdiff)
deriveSemiring(CDouble)
deriveSemiring(CFloat)
deriveSemiring(CULLong)
deriveSemiring(CLLong)
deriveSemiring(CULong)
deriveSemiring(CLong)
deriveSemiring(CUInt)
deriveSemiring(CInt)
deriveSemiring(CUShort)
deriveSemiring(CShort)
deriveSemiring(CUChar)
deriveSemiring(CSChar)
deriveSemiring(CChar)
deriveSemiring(IntPtr)
deriveSemiring(WordPtr)

#if !HOST_OS_WINDOWS
deriveSemiring(CCc)
deriveSemiring(CDev)
deriveSemiring(CGid)
deriveSemiring(CIno)
deriveSemiring(CMode)
deriveSemiring(CNlink)
deriveSemiring(COff)
deriveSemiring(CPid)
deriveSemiring(CRLim)
deriveSemiring(CSpeed)
deriveSemiring(CSsize)
deriveSemiring(CTcflag)
deriveSemiring(CUid)
deriveSemiring(Fd)
#endif

deriveSemiring(Natural)

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
#if MIN_VERSION_base(4,6,0)
deriving instance Semiring a => Semiring (Down a)
#endif
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

#define deriveRing(ty)          \
instance Ring (ty) where {      \
  negate = Num.negate           \
; {-# INLINE negate #-}         \
}

deriveRing(Int)
deriveRing(Int8)
deriveRing(Int16)
deriveRing(Int32)
deriveRing(Int64)
deriveRing(Integer)
deriveRing(Word)
deriveRing(Word8)
deriveRing(Word16)
deriveRing(Word32)
deriveRing(Word64)
deriveRing(Float)
deriveRing(Double)
deriveRing(CUIntMax)
deriveRing(CIntMax)
deriveRing(CUIntPtr)
deriveRing(CIntPtr)
deriveRing(CSUSeconds)
deriveRing(CUSeconds)
deriveRing(CTime)
deriveRing(CClock)
deriveRing(CSigAtomic)
deriveRing(CWchar)
deriveRing(CSize)
deriveRing(CPtrdiff)
deriveRing(CDouble)
deriveRing(CFloat)
deriveRing(CULLong)
deriveRing(CLLong)
deriveRing(CULong)
deriveRing(CLong)
deriveRing(CUInt)
deriveRing(CInt)
deriveRing(CUShort)
deriveRing(CShort)
deriveRing(CUChar)
deriveRing(CSChar)
deriveRing(CChar)
deriveRing(IntPtr)
deriveRing(WordPtr)

#if !HOST_OS_WINDOWS
deriveRing(CCc)
deriveRing(CDev)
deriveRing(CGid)
deriveRing(CIno)
deriveRing(CMode)
deriveRing(CNlink)
deriveRing(COff)
deriveRing(CPid)
deriveRing(CRLim)
deriveRing(CSpeed)
deriveRing(CSsize)
deriveRing(CTcflag)
deriveRing(CUid)
deriveRing(Fd)
#endif

instance Integral a => Ring (Ratio a) where
  negate = Num.negate
  {-# INLINE negate #-}

#if MIN_VERSION_base(4,6,0)
deriving instance Ring a => Ring (Down a)
#endif
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

#if MIN_VERSION_base(4,7,0)
-- | Wrapper to mimic 'Set' ('Data.Semigroup.Sum' 'Int'),
-- 'Set' ('Data.Semigroup.Product' 'Int'), etc.,
-- while having a more efficient underlying representation.
newtype IntSetOf a = IntSetOf { getIntSet :: IntSet }
  deriving
    ( Eq
#if MIN_VERSION_base(4,6,1)
    , Generic
    , Generic1
#endif
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
#endif

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

#if MIN_VERSION_base(4,7,0)
-- | Wrapper to mimic 'Map' ('Data.Semigroup.Sum' 'Int') v,
-- 'Map' ('Data.Semigroup.Product' 'Int') v, etc.,
-- while having a more efficient underlying representation.
newtype IntMapOf k v = IntMapOf { getIntMap :: IntMap v }
  deriving
    ( Eq
#if MIN_VERSION_base(4,6,1)
    , Generic
    , Generic1
#endif
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
