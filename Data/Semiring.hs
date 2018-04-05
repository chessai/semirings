{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- this is here because of Generics
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall #-}

-- this is here because of -XDefaultSignatures
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.Semiring 
  ( Semiring(..)
  , (+)
  , (*)
  , (^)
  , (^^^)
  , (+++)
  , (***)
  , square 
  , foldMapP
  , foldMapT
  , semisum
  , semiprod
  , semisum'
  , semiprod'
  , WrappedApplicative(..) 
  ) where

import           Control.Monad (MonadPlus)
import           Control.Applicative (Alternative(..), Applicative(..), Const(..))
import           Data.Bool (Bool(..), (||), (&&))
import           Data.Fixed (Fixed, HasResolution)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (Identity(..))
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (Maybe(..))
import           Data.Monoid (Dual(..), Endo(..), Alt(..), Product(..), Sum(..))
import           Data.Ord (Down(..), Ord(..), Ordering(..), compare)
import           Data.Ratio (Ratio)
import           Data.Semigroup (Max(..), Min(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Foreign.C.Types
  (CChar, CClock, CDouble, CFloat, CInt,
   CIntMax, CIntPtr, CLLong, CLong,
   CPtrdiff, CSChar, CSUSeconds, CShort,
   CSigAtomic, CSize, CTime, CUChar, CUInt,
   CUIntMax, CUIntPtr, CULLong, CULong,
   CUSeconds, CUShort, CWchar)
import           Foreign.Ptr (IntPtr, WordPtr)
import           GHC.Generics
import           Numeric.Log (Log(..), Precise)
import           Numeric.Log.Signed (SignedLog(..))
import           Numeric.Natural (Natural)
import qualified Prelude as P
import           Prelude (Eq(..))
import           Prelude (IO, Integral, Integer, Float, Double)
import           Prelude ((.), ($), flip, otherwise)
import           System.Posix.Types
  (CCc, CDev, CGid, CIno, CMode, CNlink,
   COff, CPid, CRLim, CSpeed, CSsize,
   CTcflag, CUid, Fd)

infixl 7 *, ***, `times`
infixl 6 +, +++, `plus`
infixr 8 ^, ^^^

-- | raise a number to a non-negative integral power
(^) :: (Semiring a, Integral b) => a -> b -> a
x0 ^ y0 | y0 P.< 0  = P.error "Negative exponent"
        | y0 P.== 0 = one
        | P.otherwise = f x0 y0
  where
    f x y | P.even y = f (x * x) (y `P.quot` 2)
          | y P.== 1 = x
          | P.otherwise = g (x * x) (y `P.quot` 2) x
    g x y z | P.even y = g (x * x) (y `P.quot` 2) z
            | y P.== 1 = x * z
            | P.otherwise = g (x * x) (y `P.quot` 2) (x * z)

(^^^) :: (Semiring a, Integral b) => a -> b -> a
(^^^) = (^)

(+), (*) :: Semiring a => a -> a -> a
(+) = plus
(*) = times

(+++), (***) :: Semiring a => a -> a -> a
(+++) = plus
(***) = times

square :: Semiring a => a -> a
square x = x * x

foldMapP, foldMapT :: (Foldable t, Semiring s) => (a -> s) -> t a -> s
foldMapP f = Foldable.foldr (plus  . f) zero
foldMapT f = Foldable.foldr (times . f) one
{-# INLINE foldMapP #-}
{-# INLINE foldMapT #-}

class GSemiring f where
  {-# MINIMAL gPlus, gZero, gTimes, gOne #-} 
  gZero  :: f a
  gOne   :: f a
  gPlus  :: f a -> f a -> f a
  gTimes :: f a -> f a -> f a

defZero, defOne   :: (Generic a, GSemiring (Rep a)) => a
defPlus, defTimes :: (Generic a, GSemiring (Rep a)) => a -> a -> a
defZero = to gZero
defOne  = to gOne
defPlus x y = to $ from x `gPlus` from y
defTimes x y = to $ from x `gTimes` from y

instance GSemiring U1 where
  gZero = U1
  gOne  = U1
  gPlus  _ _ = U1
  gTimes _ _ = U1

instance (GSemiring a, GSemiring b) => GSemiring (a :*: b) where
  gZero = gZero :*: gZero
  gOne  = gOne  :*: gOne
  gPlus  (a :*: b) (c :*: d) = gPlus  a c :*: gPlus b d
  gTimes (a :*: b) (c :*: d) = gTimes a c :*: gPlus b d

-- i do not intend to support sum types
--
--instance (GSemiring a, GSemiring b) => GSemiring (a :+: b) where
--  gZero = L1 gZero
--  gOne  = L1 gOne
--  gPlus (L1 x) (L1 y) = L1 (gPlus x y)
--  gPlus (R1 x) (R1 y) = R1 (gPlus x y)
--  gPlus x _ = x
--  gTimes (L1 x) (L1 y) = L1 (gTimes x y)
--  gTimes (R1 x) (R1 y) = R1 (gTimes x y)
--  gTimes x _ = x

instance (GSemiring a) => GSemiring (M1 i c a) where
  gZero = M1 gZero
  gOne  = M1 gOne
  gPlus  (M1 x) (M1 y) = M1 $ gPlus  x y
  gTimes (M1 x) (M1 y) = M1 $ gTimes x y

instance (Semiring a) => GSemiring (K1 i a) where
  gZero = K1 zero
  gOne  = K1 one
  gPlus  (K1 x) (K1 y) = K1 $ plus  x y
  gTimes (K1 x) (K1 y) = K1 $ times x y

semisum, semiprod :: (Foldable t, Semiring a) => t a -> a
semisum  = Foldable.foldr plus zero
semiprod = Foldable.foldr times one

semisum', semiprod' :: (Foldable t, Semiring a) => t a -> a
semisum'  = Foldable.foldr' plus zero
semiprod' = Foldable.foldr' times one

class Semiring a where
  {-# MINIMAL plus, zero, times, one #-}
  plus  :: a -> a -> a -- ^ Associative Additive Operation
  zero  :: a           -- ^ Additive Unit
  times :: a -> a -> a -- ^ Associative Multiplicative Operation
  one   :: a           -- ^ Multiplicative Unit

  -- useful for defining semirings over ground types
  default zero  :: P.Num a => a
  default one   :: P.Num a => a
  default plus  :: P.Num a => a -> a -> a
  default times :: P.Num a => a -> a -> a
  zero  = 0
  one   = 1
  plus  = (P.+)
  times = (P.*)

instance Semiring b => Semiring (a -> b) where
  plus f g x  = f x `plus` g x
  zero        = \_ -> zero
  times f g x = f x `times` g x
  one         = \_ -> one

instance Semiring () where
  plus _ _  = ()
  zero      = ()
  times _ _ = ()
  one       = ()

instance Semiring a => Semiring [a] where
  zero = []
  one  = [one]
  plus  = listAdd
  times = listTimes

listAdd, listTimes :: Semiring a => [a] -> [a] -> [a]
listAdd [] ys = ys
listAdd xs [] = xs
listAdd (x:xs) (y:ys) = (x + y) : listAdd xs ys
{-# NOINLINE [0] listAdd #-}

listTimes [] (_:xs) = zero : listTimes [] xs
listTimes (_:xs) [] = zero : listTimes [] xs
listTimes [] _ = []
--listTimes _ [] = []
listTimes (x:xs) (y:ys) = (x * y) : listTimes xs ys
{-# NOINLINE [0] listTimes #-}

instance Semiring a => Semiring (Vector a) where
  zero  = Vector.empty
  one   = Vector.singleton one
  plus xs ys =
    case compare (Vector.length xs) (Vector.length ys) of
      EQ -> Vector.zipWith (+) xs ys
      LT -> Vector.unsafeAccumulate (+) ys (Vector.indexed xs)
      GT -> Vector.unsafeAccumulate (+) xs (Vector.indexed ys)
  times xs ys
    | Vector.null xs = Vector.empty
    | Vector.null ys = Vector.empty
    | otherwise = Vector.generate maxlen f
      where
        f n = Foldable.foldl'
          (\_ k -> 
            Vector.unsafeIndex xs k *
            Vector.unsafeIndex ys (n P.- k)) zero [kmin .. kmax]
          where
            !kmin = P.max 0 (n P.- (klen P.- 1))
            !kmax = P.min n (slen P.- 1)
        !slen = Vector.length xs
        !klen = Vector.length ys
        !maxlen = P.max slen klen

instance (UV.Unbox a, Semiring a) => Semiring (UV.Vector a) where
  zero = UV.empty
  one  = UV.singleton one
  plus xs ys =
    case compare (UV.length xs) (UV.length ys) of
      EQ -> UV.zipWith (+) xs ys
      LT -> UV.unsafeAccumulate (+) ys (UV.indexed xs)
      GT -> UV.unsafeAccumulate (+) xs (UV.indexed ys)
  times xs ys
    | UV.null xs = UV.empty
    | UV.null ys = UV.empty
    | otherwise = UV.generate maxlen f
      where
        f n = Foldable.foldl'
          (\_ k -> 
            UV.unsafeIndex xs k *
            UV.unsafeIndex ys (n P.- k)) zero [kmin .. kmax]
          where
            !kmin = P.max 0 (n P.- (klen P.- 1))
            !kmax = P.min n (slen P.- 1)
        !slen = UV.length xs
        !klen = UV.length ys
        !maxlen = P.max slen klen

instance (SV.Storable a, Semiring a) => Semiring (SV.Vector a) where
  zero = SV.empty
  one  = SV.singleton one
  plus xs ys =
    case compare lxs lys of
      EQ -> SV.zipWith (+) xs ys
      LT -> SV.unsafeAccumulate_ (+) ys (SV.enumFromN 0 lxs) xs
      GT -> SV.unsafeAccumulate_ (+) xs (SV.enumFromN 0 lys) ys
    where
      lxs = SV.length xs
      lys = SV.length ys
  times xs ys
    | SV.null xs = SV.empty
    | SV.null ys = SV.empty
    | otherwise  = SV.generate maxlen f
      where
        f n = Foldable.foldl'
          (\_ k -> 
            SV.unsafeIndex xs k *
            SV.unsafeIndex ys (n P.- k)) zero [kmin .. kmax]
          where
            !kmin = P.max 0 (n P.- (klen P.- 1))
            !kmax = P.min n (slen P.- 1)
        !slen = SV.length xs
        !klen = SV.length ys
        !maxlen = P.max slen klen

instance Semiring Bool where
  plus  = (||)
  zero  = False
  times = (&&)
  one   = True

instance Semiring a => Semiring (Maybe a) where
  zero  = Nothing
  one   = Just one

  plus Nothing y = y
  plus x Nothing = x
  plus (Just x) (Just y) = Just (plus x y)

  times Nothing _ = Nothing
  times _ Nothing = Nothing
  times (Just x) (Just y) = Just (times x y)

instance Semiring a => Semiring (IO a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times

instance Semiring a => Semiring (Dual a) where
  zero = Dual zero
  Dual x `plus` Dual y = Dual (y `plus` x)
  one = Dual one
  Dual x `times` Dual y = Dual (y `times` x)

-- | This is not a true semiring. Even if the underlying
-- monoid is commutative, it is only a near semiring. It
-- is, however, quite useful. For instance, this type:
--
-- @forall a. 'Endo' ('Endo' a)@
--
-- is a valid encoding of church numerals, with addition and
-- multiplication being their semiring variants.
deriving newtype instance Semiring a => Semiring (Endo a)

newtype WrappedApplicative f a = WrappedApplicative { getApplicative :: f a }
  deriving (Generic, Generic1, P.Read, P.Show, Eq, Ord, P.Num,
            P.Enum, P.Monad, MonadPlus, Applicative,
            Alternative, P.Functor)

instance (Applicative f, Semiring a) => Semiring (WrappedApplicative f a) where
  zero  = WrappedApplicative (pure zero)
  one   = WrappedApplicative (pure one)
  plus  = liftA2 plus
  times = liftA2 times

instance (Alternative f, Semiring a) => Semiring (Alt f a) where
  zero  = empty
  one   = Alt (pure one)
  plus  = (<|>)
  times = liftA2 times

instance Semiring a => Semiring (Const a b) where
  zero = Const zero
  one  = Const one
  plus  (Const x) (Const y) = Const (x `plus`  y)
  times (Const x) (Const y) = Const (x `times` y)

instance (Ord a, Semiring a) => Semiring (Set a) where
  zero  = Set.empty
  one   = Set.singleton one
  plus  = Set.union
#if MIN_VERSION_containers(5,11,0)
  times xs ys = Set.map (P.uncurry times) (Set.cartesianProduct xs ys)
#else
  -- I think this could also be 'times xs ys = foldMap (flip Set.map ys . mappend) xs'
  times xs ys = Set.fromList (times (Set.toList xs) (Set.toList ys))
#endif

instance (Ord a, Semiring a, Semiring b) => Semiring (Map a b) where
  zero = Map.empty
  one  = Map.singleton zero one
  plus = Map.unionWith (+)
  xs `times` ys
    = Map.fromListWith (+)
        [ (plus k l, v * u)
        | (k,v) <- Map.toList xs
        , (l,u) <- Map.toList ys ]

instance (Eq a, Hashable a, Semiring a) => Semiring (HashSet a) where
  zero = HashSet.empty
  one  = HashSet.singleton one
  plus = HashSet.union
  times xs ys = foldMapT (flip HashSet.map ys . times) xs

instance (Eq k, Hashable k, Semiring k, Semiring v) => Semiring (HashMap k v) where
  zero = HashMap.empty
  one  = HashMap.singleton zero one
  plus = HashMap.unionWith (+)
  xs `times` ys
    = HashMap.fromListWith (+)
        [ (k + l, v * u)
        | (k,v) <- HashMap.toList xs
        , (l,u) <- HashMap.toList ys ]

instance (Precise a, P.RealFloat a) => Semiring (Log a) where
  zero  = Exp (-(1 P./ 0))
  one   = Exp 0
  plus  = (P.+)
  times = (P.*)

instance (Precise a, P.RealFloat a) => Semiring (SignedLog a) where
  zero  = SLExp False (-(1 P./ 0))
  one   = SLExp True 0
  plus  = (P.+)
  times = (P.*)

instance (Semiring a, Semiring b) => Semiring (a,b) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes; 

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a,b,c,d,e) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f) => Semiring (a,b,c,d,e,f) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes; 

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g) => Semiring (a,b,c,d,e,f,g) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Integer
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
instance Semiring Float
instance Semiring Double
instance Semiring CUIntMax
instance Semiring CIntMax
instance Semiring CUIntPtr
instance Semiring CIntPtr
instance Semiring CSUSeconds
instance Semiring CUSeconds
instance Semiring CTime
instance Semiring CClock
instance Semiring CSigAtomic
instance Semiring CWchar
instance Semiring CSize
instance Semiring CPtrdiff
instance Semiring CDouble
instance Semiring CFloat
instance Semiring CULLong
instance Semiring CLLong
instance Semiring CULong
instance Semiring CLong
instance Semiring CUInt
instance Semiring CInt
instance Semiring CUShort
instance Semiring CShort
instance Semiring CUChar
instance Semiring CSChar
instance Semiring CChar
instance Semiring IntPtr
instance Semiring WordPtr
instance Semiring Fd
instance Semiring CRLim
instance Semiring CTcflag
instance Semiring CSpeed
instance Semiring CCc
instance Semiring CUid
instance Semiring CNlink
instance Semiring CGid
instance Semiring CSsize
instance Semiring CPid
instance Semiring COff
instance Semiring CMode
instance Semiring CIno
instance Semiring CDev
instance Semiring Natural
instance Integral a => Semiring (Ratio a)
deriving instance Semiring a => Semiring (Product a)
deriving instance Semiring a => Semiring (Sum a)
deriving instance Semiring a => Semiring (Identity a)
deriving instance Semiring a => Semiring (Down a)
deriving instance Semiring a => Semiring (Max a)
deriving instance Semiring a => Semiring (Min a)
instance HasResolution a => Semiring (Fixed a)
