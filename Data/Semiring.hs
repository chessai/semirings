{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- A class for semirings (types with two binary operations, one commutative and one associative, and two respective identites), with various general-purpose instances.
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

    -- * Types
  , Add(..)
  , Mul(..)

    -- * Ring typeclass 
  , Ring(..)
  , (-)
  , minus 
  ) where 

import           Control.Applicative (Applicative(..), Const(..), liftA2)
import           Data.Bool (Bool(..), (||), (&&), otherwise, not)
import           Data.Complex (Complex(..))
import           Data.Eq (Eq(..))
import           Data.Fixed (Fixed(MkFixed), HasResolution)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Function ((.), const, flip, id)
import           Data.Functor (Functor(..))
#if MIN_VERSION_base(4,12,0)
import           Data.Functor.Contravariant (Predicate(..), Comparison(..), Equivalence(..), Op(..))
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
--import           Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
--import           Data.IntSet (IntSet)
--import qualified Data.IntSet as IntSet
import           Data.Map (Map)
import qualified Data.Map as Map
#endif
import           Data.Monoid (Monoid(..),Dual(..), Product(..), Sum(..))
import           Data.Ord (Ord(..), Ordering(..), compare)
#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down(..))
#endif
import           Data.Proxy (Proxy(..))
import           Data.Ratio (Ratio, Rational, (%))
import           Data.Semigroup (Semigroup(..),Max(..), Min(..))
#if defined(VERSION_containers)
import           Data.Set (Set)
import qualified Data.Set as Set
#endif
-- #if defined(VERSION_primitive)
-- import           Data.Primitive.Array (Array(..))
-- import qualified Data.Primitive.Array as Array
-- #endif
import           Data.Traversable (Traversable)
import           Data.Typeable (Typeable)
#if defined(VERSION_vector)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
#endif
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
import           GHC.Base (build)
import           GHC.Enum (Enum, Bounded)
import           GHC.Float (Float, Double)
#if MIN_VERSION_base(4,6,1)
import           GHC.Generics (Generic,Generic1)
#endif
import           GHC.IO (IO)
import           GHC.Integer (Integer)
import qualified GHC.Num as Num
import           GHC.Read (Read)
import           GHC.Real (Integral, Fractional, Real, RealFrac, quot, even)
import           GHC.Show (Show)
import           Numeric.Natural (Natural)
import           System.Posix.Types
  (CCc, CDev, CGid, CIno, CMode, CNlink,
   COff, CPid, CRLim, CSpeed, CSsize,
   CTcflag, CUid, Fd)

infixl 7 *, `times`
infixl 6 +, `plus`, -, `minus`
infixr 8 ^

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}

-- | Raise a number to a non-negative integral power.
-- If the power is negative, this will return 'zero'.
(^) :: (Semiring a, Integral b) => a -> b -> a
x0 ^ y0 | y0 < 0  = zero
        | y0 == 0 = one
        | otherwise = f x0 y0
  where
    f x y | even y = f (x * x) (y `quot` 2)
          | y == 1 = x
          | otherwise = g (x * x) (y `quot` 2) x
    g x y z | even y = g (x * x) (y `quot` 2) z
            | y == 1 = x * z
            | otherwise = g (x * x) (y `quot` 2) (x * z)
{-# INLINE (^) #-}

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

-- | The 'sum' function computes the additive sum of the elements in a structure.
--   This function is lazy. For a strict version, see 'sum''.
sum  :: (Foldable t, Semiring a) => t a -> a
sum  = Foldable.foldr plus zero
{-# INLINE sum #-}

-- | The 'product' function computes the product of the elements in a structure.
--   This function is lazy. for a strict version, see 'product''.
product :: (Foldable t, Semiring a) => t a -> a
product = Foldable.foldr times one
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
    , Semiring
    , Show
    , Storable
    , Traversable
    , Typeable
    )

instance Semiring a => Semigroup (Add a) where
  (<>) = (+)
  {-# INLINE (<>) #-}

instance Semiring a => Monoid (Add a) where
  mempty = Add zero
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

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
    , Semiring
    , Show
    , Storable
    , Traversable
    , Typeable
    )

instance Semiring a => Semigroup (Mul a) where
  (<>) = (*)
  {-# INLINE (<>) #-}

instance Semiring a => Monoid (Mul a) where
  mempty = Mul one
  mappend = (<>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

{--------------------------------------------------------------------
  Classes
--------------------------------------------------------------------}

-- | The class of semirings (types with two binary
-- operations and two respective identities). One
-- can think of a semiring as two monoids of the same
-- underlying type: A commutative monoid and an
-- associative monoid. For any type R with a 'Prelude.Num'
-- instance, the commutative monoid is (R, '(Prelude.+)', 0)
-- and the associative monoid is (R, '(Prelude.*)', 1).
--
-- Instances should satisfy the following laws:
--
-- [/additive identity/]
-- 
--     @x '+' 'zero' = 'zero' '+' x = x@
-- 
-- [/additive associativity/]
-- 
--     @x '+' (y '+' z) = (x '+' y) '+' z@
--
-- [/additive commutativity/]
--     
--     @x '+' y = y '+' x@
--
-- [/multiplicative identity/]
-- 
--     @x '*' 'one' = 'one' '*' x = x@
--
-- [/multiplicative associativity/]
--
--     @x '*' (y '*' z) = (x '*' y) '*' z@
-- 
-- [/left- and right-distributivity of '*' over '+'/]
--
--     @x '*' (y '+' z) = (x '*' y) '+' (x '*' z)@
--     @(x '+' y) '*' z = (x '*' z) '+' (y '*' z)@
--
-- [/annihilation/]
--
--     @'zero' '*' x = x '*' 'zero' = 'zero'@

class Semiring a where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL plus, zero, times, one #-}
#endif
  plus  :: a -> a -> a -- ^ Commutative Operation
  zero  :: a           -- ^ Commutative Unit
  times :: a -> a -> a -- ^ Associative Operation
  one   :: a           -- ^ Associative Unit

-- | The class of semirings with an additive inverse.
--
--     @'negate' a '+' a = 'zero'@

class Semiring a => Ring a where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL negate #-}
#endif
  negate :: a -> a

-- | Substract two 'Ring' values. For any type 'R' with
-- a 'Prelude.Num' instance, this is the same as '(Prelude.-)'.
--
--     @x `minus` y = x '+' 'negate' y@
minus :: Ring a => a -> a -> a
minus x y = x + negate y
{-# INLINE minus #-}

{--------------------------------------------------------------------
  Instances (base)
--------------------------------------------------------------------}

instance Semiring b => Semiring (a -> b) where
  plus f g x  = f x `plus` g x
  zero        = const zero
  times f g x = f x `times` g x
  one         = const one
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring b => Ring (a -> b) where
  negate f x = negate (f x)
  {-# INLINE negate #-}

instance Semiring () where
  plus _ _  = ()
  zero      = ()
  times _ _ = ()
  one       = ()
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring () where
  negate _ = ()
  {-# INLINE negate #-}

instance Semiring (Proxy a) where
  plus _ _  = Proxy
  zero      = Proxy
  times _ _ = Proxy
  one       = Proxy
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Semiring Bool where
  plus  = (||)
  zero  = False
  times = (&&)
  one   = True
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring Bool where
  negate = not
  {-# INLINE negate #-}

-- See Section: List fusion
instance Semiring a => Semiring [a] where
  zero = []
  one  = [one]
  plus  = listAdd
  times = listTimes
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring a => Ring [a] where
  negate = fmap negate
  {-# INLINE negate #-}

instance Semiring a => Semiring (Maybe a) where
  zero  = Nothing
  one   = Just one

  plus Nothing y = y
  plus x Nothing = x
  plus (Just x) (Just y) = Just (plus x y)

  times Nothing _ = Nothing
  times _ Nothing = Nothing
  times (Just x) (Just y) = Just (times x y)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring a => Ring (Maybe a) where
  negate = fmap negate
  {-# INLINE negate #-}

instance Semiring a => Semiring (IO a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring a => Ring (IO a) where
  negate = fmap negate
  {-# INLINE negate #-}

instance Semiring a => Semiring (Dual a) where
  zero = Dual zero
  Dual x `plus` Dual y = Dual (y `plus` x)
  one = Dual one
  Dual x `times` Dual y = Dual (y `times` x)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring a => Ring (Dual a) where
  negate (Dual x) = Dual (negate x)
  {-# INLINE negate #-}

instance Semiring a => Semiring (Const a b) where
  zero = Const zero
  one  = Const one
  plus  (Const x) (Const y) = Const (x `plus`  y)
  times (Const x) (Const y) = Const (x `times` y)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

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
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring a => Ring (Complex a) where
  negate (x :+ y) = negate x :+ negate y
  {-# INLINE negate #-}

#if MIN_VERSION_base(4,12,0)
instance (Semiring a, Applicative f) => Semiring (Ap f a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance (Ring a, Applicative f) => Ring (Ap f a) where
  negate = fmap negate
  {-# INLINE negate #-}
#endif

#if MIN_VERSION_base(4,12,0)
deriving instance Semiring (Predicate a)
deriving instance Ring (Predicate a)

deriving instance Semiring a => Semiring (Equivalence a)
deriving instance Ring a => Ring (Equivalence a)

deriving instance Semiring a => Semiring (Op a b)
deriving instance Ring a => Ring (Op a b)
#endif

#define deriveSemiring(ty)        \
instance Semiring (ty) where {    \
   zero  = 0                      \
;  one   = 1                      \
;  plus  x y = (Num.+) x y        \
;  times x y = (Num.*) x y        \
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
deriveSemiring(Fd)
deriveSemiring(CRLim)
deriveSemiring(CTcflag)
deriveSemiring(CSpeed)
deriveSemiring(CCc)
deriveSemiring(CUid)
deriveSemiring(CNlink)
deriveSemiring(CGid)
deriveSemiring(CSsize)
deriveSemiring(CPid)
deriveSemiring(COff)
deriveSemiring(CMode)
deriveSemiring(CIno)
deriveSemiring(CDev)
deriveSemiring(Natural)
instance Integral a => Semiring (Ratio a) where
  {-# SPECIALIZE instance Semiring Rational #-} 
  zero  = 0 % 1
  one   = 1 % 1
  plus  = (Num.+)
  times = (Num.*)
  {-# INLINE zero  #-}
  {-# INLINE one   #-}
  {-# INLINE plus  #-}
  {-# INLINE times #-}
deriving instance Semiring a => Semiring (Product a)
deriving instance Semiring a => Semiring (Sum a)
deriving instance Semiring a => Semiring (Identity a)
#if MIN_VERSION_base(4,6,0)
deriving instance Semiring a => Semiring (Down a)
#endif
deriving instance Semiring a => Semiring (Max a)
deriving instance Semiring a => Semiring (Min a)
instance HasResolution a => Semiring (Fixed a) where
  zero  = MkFixed 0
  one   = MkFixed 1
  plus  = (Num.+)
  times = (Num.*)
  {-# INLINE zero  #-}
  {-# INLINE one   #-}
  {-# INLINE plus  #-}
  {-# INLINE times #-}

#define deriveRing(ty)          \
instance Ring (ty) where {      \
  negate = Num.negate           \
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
deriveRing(Fd)
deriveRing(CRLim)
deriveRing(CTcflag)
deriveRing(CSpeed)
deriveRing(CCc)
deriveRing(CUid)
deriveRing(CNlink)
deriveRing(CGid)
deriveRing(CSsize)
deriveRing(CPid)
deriveRing(COff)
deriveRing(CMode)
deriveRing(CIno)
deriveRing(CDev)
deriveRing(Natural)
instance Integral a => Ring (Ratio a) where
  negate = Num.negate
  {-# INLINE negate #-}

#if MIN_VERSION_base(4,6,0)
deriving instance Ring a => Ring (Down a)
#endif
deriving instance Ring a => Ring (Product a)
deriving instance Ring a => Ring (Sum a)
deriving instance Ring a => Ring (Identity a)
deriving instance Ring a => Ring (Max a)
deriving instance Ring a => Ring (Min a)
instance HasResolution a => Ring (Fixed a) where
  negate = Num.negate
  {-# INLINE negate #-}

{--------------------------------------------------------------------
  Instances (containers)
--------------------------------------------------------------------}

#if defined(VERSION_containers)

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid', so we require a
--   'Monoid' contraint instead of a 'Semiring'
--   constraint since 'times' can use
--   the context of either.
instance (Ord a, Monoid a) => Semiring (Set a) where
  zero  = Set.empty
  one   = Set.singleton mempty
  plus  = Set.union
  times xs ys = Foldable.foldMap (flip Set.map ys . mappend) xs
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid' as the key type,
--   so we require a 'Monoid' contraint instead of
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
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

--newtype IntSetP = IntSetP { intSetP :: IntSet }
--newtype IntSetT = IntSetT { intSetT :: IntSet }
--
--instance Semiring IntSetP where
--  zero = IntSetP (IntSet.empty)
--  one  = IntSetP (IntSet.singleton zero)
--  plus (IntSetP x) (IntSetP y) = IntSetP (IntSet.union x y)
--  times (IntSetP xs) (IntSetP ys) = IntSetP (foldMapIntSet (flip IntSet.map ys . plus) xs)
--
--instance Semiring IntSetT where
--  zero = IntSetT IntSet.empty
--  one  = IntSetT (IntSet.singleton one)
--  plus (IntSetT x) (IntSetT y) = IntSetT (IntSet.union x y)
--  times (IntSetT xs) (IntSetT ys) = IntSetT (foldMapIntSet (flip IntSet.map ys . times) xs)
--
--foldMapIntSet :: Monoid m => (Int -> m) -> IntSet -> m
--foldMapIntSet f = IntSet.foldl' (flip (mappend . f)) mempty
--{-# INLINE foldMapIntSet #-}

--instance (Semiring a) => Semiring (IntMap a) where
--  zero = IntMap.empty
--  one  = IntMap.singleton zero one
--  plus = IntMap.unionWith (+)
--  xs `times` ys
--    = IntMap.fromListWith (+)
--        [ (plus k l, v * u)
--        | (k,v) <- IntMap.toList xs
--        , (l,u) <- IntMap.toList ys
--        ]
#endif

{--------------------------------------------------------------------
  Instances (unordered-containers)
--------------------------------------------------------------------}

#if defined(VERSION_unordered_containers)

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid', so we require a
--   'Monoid' contraint instead of a 'Semiring'
--   constraint since 'times' can use
--   the context of either.
instance (Eq a, Hashable a, Monoid a) => Semiring (HashSet a) where
  zero = HashSet.empty
  one  = HashSet.singleton mempty
  plus = HashSet.union
  times xs ys = Foldable.foldMap (flip HashSet.map ys . mappend) xs
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

-- | The multiplication laws are satisfied for
--   any underlying 'Monoid' as the key type,
--   so we require a 'Monoid' contraint instead of
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
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
#endif

{--------------------------------------------------------------------
  Instances (primitive)
--------------------------------------------------------------------}

#if defined(VERSION_primitive)
-- | The multiplication laws are satisfied for
--   any underlying 'Monoid', so we require a
--   'Monoid' contraint instead of a 'Semiring'
--   constraint since 'times' can use
--   the context of either.
-- instance (Monoid a) => Semiring (Array a) where
--   zero  = mempty
--   one   = runST e where
--     e :: forall s. Monoid a => ST s (Array a)
--     e = (Array.newArray 1 mempty) >>= Array.unsafeFreezeArray
--   plus _ _ = mempty
--   times _ _ = mempty
--   {-# INLINE plus  #-}
--   {-# INLINE zero  #-}
--   {-# INLINE times #-}
--   {-# INLINE one   #-}
#endif

{--------------------------------------------------------------------
  Instances (vector)
--------------------------------------------------------------------}

#if defined(VERSION_vector)
instance Semiring a => Semiring (Vector a) where
  zero  = Vector.empty
  one   = Vector.singleton one
  plus xs ys =
    case compare (Vector.length xs) (Vector.length ys) of
      EQ -> Vector.zipWith (+) xs ys
      LT -> Vector.unsafeAccumulate (+) ys (Vector.indexed xs)
      GT -> Vector.unsafeAccumulate (+) xs (Vector.indexed ys)
  times signal kernel
    | Vector.null signal = Vector.empty
    | Vector.null kernel = Vector.empty
    | otherwise = Vector.generate (slen + klen - 1) f
    where
      !slen = Vector.length signal
      !klen = Vector.length kernel
      f n = Foldable.foldl'
        (\a k -> a +
                 Vector.unsafeIndex signal k *
                 Vector.unsafeIndex kernel (n - k)
        )
        zero
        [kmin .. kmax]
        where
          !kmin = max 0 (n - (klen - 1))
          !kmax = min n (slen - 1)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}

instance Ring a => Ring (Vector a) where
  negate = Vector.map negate
  {-# INLINE negate #-}

instance (UV.Unbox a, Semiring a) => Semiring (UV.Vector a) where
  zero = UV.empty
  one  = UV.singleton one
  plus xs ys =
    case compare (UV.length xs) (UV.length ys) of
      EQ -> UV.zipWith (+) xs ys
      LT -> UV.unsafeAccumulate (+) ys (UV.indexed xs)
      GT -> UV.unsafeAccumulate (+) xs (UV.indexed ys)
  times signal kernel
    | UV.null signal = UV.empty
    | UV.null kernel = UV.empty
    | otherwise = UV.generate (slen + klen - 1) f
    where
      !slen = UV.length signal
      !klen = UV.length kernel
      f n = Foldable.foldl'
        (\a k -> a +
                 UV.unsafeIndex signal k *
                 UV.unsafeIndex kernel (n - k)
        )
        zero
        [kmin .. kmax]
        where
          !kmin = max 0 (n - (klen - 1))
          !kmax = min n (slen - 1)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
 
instance (UV.Unbox a, Ring a) => Ring (UV.Vector a) where
  negate = UV.map negate
  {-# INLINE negate #-}

instance (SV.Storable a, Semiring a) => Semiring (SV.Vector a) where
  zero = SV.empty
  one = SV.singleton one
  plus xs ys =
    case compare lxs lys of
      EQ -> SV.zipWith (+) xs ys
      LT -> SV.unsafeAccumulate_ (+) ys (SV.enumFromN 0 lxs) xs
      GT -> SV.unsafeAccumulate_ (+) xs (SV.enumFromN 0 lys) ys
    where
      lxs = SV.length xs
      lys = SV.length ys
  times signal kernel
    | SV.null signal = SV.empty
    | SV.null kernel = SV.empty
    | otherwise = SV.generate (slen + klen - 1) f
      where
        !slen = SV.length signal
        !klen = SV.length kernel
        f n = Foldable.foldl'
          (\a k -> a +
                  SV.unsafeIndex signal k *
                  SV.unsafeIndex kernel (n - k))
                zero
                [kmin .. kmax]
          where
            !kmin = max 0 (n - (klen - 1))
            !kmax = min n (slen - 1)
  {-# INLINE plus  #-}
  {-# INLINE zero  #-}
  {-# INLINE times #-}
  {-# INLINE one   #-}
  
instance (SV.Storable a, Ring a) => Ring (SV.Vector a) where
  negate = SV.map negate
  {-# INLINE negate #-}
#endif

-- [Section: List fusion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
listAdd, listTimes :: Semiring a => [a] -> [a] -> [a]
listAdd [] ys = ys
listAdd xs [] = xs
listAdd (x:xs) (y:ys) = (x + y) : listAdd xs ys
{-# NOINLINE [0] listAdd #-}

listTimes [] (_:xs) = zero : listTimes [] xs
listTimes (_:xs) [] = zero : listTimes xs []
listTimes [] [] = []
listTimes (x:xs) (y:ys) = (x * y) : listTimes xs ys
{-# NOINLINE [0] listTimes #-}

type ListBuilder a = forall b. (a -> b -> b) -> b -> b

{-# RULES 
"listAddFB/left"  forall    (g :: ListBuilder a). listAdd    (build g) = listAddFBL g
"listAddFB/right" forall xs (g :: ListBuilder a). listAdd xs (build g) = listAddFBR xs g
  #-}

-- a definition of listAdd which can be fused on its left argument
listAddFBL :: Semiring a => ListBuilder a -> [a] -> [a]
listAddFBL xf = xf f id where
  f x xs (y:ys) = x + y : xs ys
  f x xs []     = x : xs []

-- a definition of listAdd which can be fused on its right argument
listAddFBR :: Semiring a => [a] -> ListBuilder a -> [a]
listAddFBR xs' yf = yf f id xs' where
  f y ys (x:xs) = x + y : ys xs
  f y ys []     = y : ys []
