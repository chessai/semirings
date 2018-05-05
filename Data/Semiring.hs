{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wall #-}

-- this is here because of -XDefaultSignatures
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.Semiring
  ( -- * Semiring typeclass
    Semiring(..)
  , (+)
  , (*)
  , (^)
  , foldMapP
  , foldMapT
  , sum
  , prod
  , sum'
  , prod'
  
    -- * Ring typeclass 
  , Ring(..)
  , (-)
  , minus 
  ) where 

#if defined(VERSION_constrictor)
import           Constrictor (Ap(..))
#endif
import           Control.Applicative (Alternative(..), Applicative(..), Const(..), liftA2)
import           Data.Bool (Bool(..), (||), (&&), otherwise, not)
import           Data.Complex (Complex(..))
import           Data.Eq (Eq(..))
import           Data.Fixed (Fixed, HasResolution)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Function ((.), const, flip, id)
import           Data.Functor (fmap)
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
#if defined(VERSION_containers)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map)
import qualified Data.Map as Map
#endif
import           Data.Monoid (Monoid(..),Dual(..), Endo(..), Product(..), Sum(..))
#if MIN_VERSION_base(4,8,0)
import           Data.Monoid (Alt(..))
#endif
import           Data.Ord (Ord(..), Ordering(..), compare)
#if MIN_VERSION_base(4,6,0)
import           Data.Ord (Down(..))
#endif
import           Data.Ratio (Ratio)
import           Data.Semigroup (Max(..), Min(..))
#if defined(VERSION_containers)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
#endif
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
import           GHC.Base (build)
import           GHC.Float (Float, Double)
import           GHC.IO (IO)
import           GHC.Integer (Integer)
import qualified GHC.Num as Num
import           GHC.Real (Integral, quot, even)
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

-- | Infix shorthand for 'plus'.
(+) :: Semiring a => a -> a -> a
(+) = plus

-- | Infix shorthand for 'times'.
(*) :: Semiring a => a -> a -> a
(*) = times

-- | Infix shorthand for 'minus'.
(-) :: Ring a => a -> a -> a
(-) = minus

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

-- | The 'prod' function computes the multiplicative sum of the elements in a structure.
--   This function is lazy. for a strict version, see 'prod''.
prod :: (Foldable t, Semiring a) => t a -> a
prod = Foldable.foldr times one

-- | The 'sum'' function computes the additive sum of the elements in a structure.
--   This function is strict. For a lazy version, see 'sum'.
sum'  :: (Foldable t, Semiring a) => t a -> a
sum'  = Foldable.foldr' plus zero

-- | The 'prod'' function computes the additive sum of the elements in a structure.
--   This function is strict. For a lazy version, see 'prod'.
prod' :: (Foldable t, Semiring a) => t a -> a
prod' = Foldable.foldr' times one

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

  -- useful for defining semirings over ground types
  default zero  :: Num.Num a => a -- ^ 0
  default one   :: Num.Num a => a -- ^ 1
  default plus  :: Num.Num a => a -> a -> a -- ^ '(Prelude.+)'
  default times :: Num.Num a => a -> a -> a -- ^ '(Prelude.*)'
  zero  = 0
  one   = 1
  plus  = (Num.+)
  times = (Num.*)

-- | The class of semirings with an additive inverse.
--
--     @'negate' a '+' a = 'zero'@

class Semiring a => Ring a where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL negate #-}
#endif
  negate :: a -> a

  default negate :: Num.Num a => a -> a
  negate = Num.negate

-- | Substract two 'Ring' values. For any type 'R' with
-- a 'Prelude.Num' instance, this is the same as '(Prelude.-)'.
--
--     @x `minus` y = x '+' 'negate' y@
minus :: Ring a => a -> a -> a
minus x y = x + negate y

{--------------------------------------------------------------------
  Instances (base)
--------------------------------------------------------------------}

instance Semiring b => Semiring (a -> b) where
  plus f g x  = f x `plus` g x
  zero        = const zero
  times f g x = f x `times` g x
  one         = const one

instance Ring b => Ring (a -> b) where
  negate f x = negate (f x)

instance Semiring () where
  plus _ _  = ()
  zero      = ()
  times _ _ = ()
  one       = ()

instance Ring () where
  negate _ = ()

instance Semiring Bool where
  plus  = (||)
  zero  = False
  times = (&&)
  one   = True

instance Ring Bool where
  negate = not

-- See Section: List fusion
instance Semiring a => Semiring [a] where
  zero = []
  one  = [one]
  plus  = listAdd
  times = listTimes

instance Ring a => Ring [a] where
  negate = fmap negate

instance Semiring a => Semiring (Maybe a) where
  zero  = Nothing
  one   = Just one

  plus Nothing y = y
  plus x Nothing = x
  plus (Just x) (Just y) = Just (plus x y)

  times Nothing _ = Nothing
  times _ Nothing = Nothing
  times (Just x) (Just y) = Just (times x y)

instance Ring a => Ring (Maybe a) where
  negate = fmap negate

instance Semiring a => Semiring (IO a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times

instance Ring a => Ring (IO a) where
  negate = fmap negate

instance Semiring a => Semiring (Dual a) where
  zero = Dual zero
  Dual x `plus` Dual y = Dual (y `plus` x)
  one = Dual one
  Dual x `times` Dual y = Dual (y `times` x)

instance Ring a => Ring (Dual a) where
  negate (Dual x) = Dual (negate x)

-- | This is not a true semiring. Even if the underlying
-- monoid is commutative, it is only a near semiring. It
-- is, however, quite useful. For instance, this type:
--
-- @forall a. 'Endo' ('Endo' a)@
--
-- is a valid encoding of church numerals, with addition and
-- multiplication being their semiring variants.
instance Monoid a => Semiring (Endo a) where
  zero  = Endo mempty
  plus (Endo f) (Endo g) = Endo (mappend f g)
  one   = mempty
  times = mappend

instance (Monoid a, Ring a) => Ring (Endo a) where
  negate (Endo f) = Endo (negate f)

#if MIN_VERSION_base(4,8,0)
instance (Alternative f, Semiring a) => Semiring (Alt f a) where
  zero  = empty
  one   = Alt (pure one)
  plus  = (<|>)
  times = liftA2 times

instance (Alternative f, Ring a) => Ring (Alt f a) where
  negate = fmap negate
#endif

instance Semiring a => Semiring (Const a b) where
  zero = Const zero
  one  = Const one
  plus  (Const x) (Const y) = Const (x `plus`  y)
  times (Const x) (Const y) = Const (x `times` y)

instance Ring a => Ring (Const a b) where
  negate (Const x) = Const (negate x)

instance Ring a => Semiring (Complex a) where
  zero = zero :+ zero
  one  = one  :+ zero
  plus  (x :+ y) (x' :+ y') = plus x x' :+ plus y y'
  times (x :+ y) (x' :+ y')
    = (x * x' - (y * y')) :+ (x * y' + y * x')

instance Ring a => Ring (Complex a) where
  negate (x :+ y) = negate x :+ negate y

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
#if MIN_VERSION_base(4,6,0)
deriving instance Semiring a => Semiring (Down a)
#endif
deriving instance Semiring a => Semiring (Max a)
deriving instance Semiring a => Semiring (Min a)
instance HasResolution a => Semiring (Fixed a)

instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance Ring Integer
instance Ring Word
instance Ring Word8
instance Ring Word16
instance Ring Word32
instance Ring Word64
instance Ring Float
instance Ring Double
instance Ring CUIntMax
instance Ring CIntMax
instance Ring CUIntPtr
instance Ring CIntPtr
instance Ring CSUSeconds
instance Ring CUSeconds
instance Ring CTime
instance Ring CClock
instance Ring CSigAtomic
instance Ring CWchar
instance Ring CSize
instance Ring CPtrdiff
instance Ring CDouble
instance Ring CFloat
instance Ring CULLong
instance Ring CLLong
instance Ring CULong
instance Ring CLong
instance Ring CUInt
instance Ring CInt
instance Ring CUShort
instance Ring CShort
instance Ring CUChar
instance Ring CSChar
instance Ring CChar
instance Ring IntPtr
instance Ring WordPtr
instance Ring Fd
instance Ring CRLim
instance Ring CTcflag
instance Ring CSpeed
instance Ring CCc
instance Ring CUid
instance Ring CNlink
instance Ring CGid
instance Ring CSsize
instance Ring CPid
instance Ring COff
instance Ring CMode
instance Ring CIno
instance Ring CDev
instance Ring Natural
instance Integral a => Ring (Ratio a)
#if MIN_VERSION_base(4,6,0)
deriving instance Ring a => Ring (Down a)
#endif
deriving instance Ring a => Ring (Product a)
deriving instance Ring a => Ring (Sum a)
deriving instance Ring a => Ring (Identity a)
deriving instance Ring a => Ring (Max a)
deriving instance Ring a => Ring (Min a)
instance HasResolution a => Ring (Fixed a)

{--------------------------------------------------------------------
  Instances (constrictor)
--------------------------------------------------------------------}

#if defined(VERSION_constrictor)
instance (Applicative f, Semiring a) => Semiring (Ap f a) where
  zero  = Ap (pure zero)
  one   = Ap (pure one)
  plus  = liftA2 plus
  times = liftA2 times
#endif

{--------------------------------------------------------------------
  Instances (containers)
--------------------------------------------------------------------}

#if defined(VERSION_containers)
instance (Ord a, Semiring a) => Semiring (Set a) where
  zero  = Set.empty
  one   = Set.singleton one
  plus  = Set.union
#if MIN_VERSION_containers(5,11,0)
  times xs ys = Set.map (P.uncurry times) (Set.cartesianProduct xs ys)
#else
  -- I think this could also be 'times xs ys = foldMapT (flip Set.map ys . times) xs'
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
        , (l,u) <- Map.toList ys
        ]

instance Semiring IntSet where
  zero = IntSet.empty
  one  = IntSet.singleton one
  plus = IntSet.union
  times xs ys = IntSet.fromList (times (IntSet.toList xs) (IntSet.toList ys))

instance (Semiring a) => Semiring (IntMap a) where
  zero = IntMap.empty
  one  = IntMap.singleton zero one
  plus = IntMap.unionWith (+)
  xs `times` ys
    = IntMap.fromListWith (+)
        [ (plus k l, v * u)
        | (k,v) <- IntMap.toList xs
        , (l,u) <- IntMap.toList ys
        ]

instance (Semiring a) => Semiring (Seq a) where
  zero = Seq.empty
  one  = Seq.singleton one
  plus = (Seq.><)
  times xs ys = Seq.fromList (times (Foldable.toList xs) (Foldable.toList ys))
#endif

{--------------------------------------------------------------------
  Instances (unordered-containers)
--------------------------------------------------------------------}

#if defined(VERSION_unordered_containers)
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
  times xs ys
    | Vector.null xs = Vector.empty
    | Vector.null ys = Vector.empty
    | otherwise = Vector.generate maxlen f
      where
        f n = Foldable.foldl'
          (\_ k -> 
            Vector.unsafeIndex xs k *
            Vector.unsafeIndex ys (n Num.- k)) zero [kmin .. kmax]
          where
            !kmin = max 0 (n Num.- (klen Num.- 1))
            !kmax = min n (slen Num.- 1)
        !slen = Vector.length xs
        !klen = Vector.length ys
        !maxlen = max slen klen

instance Ring a => Ring (Vector a) where
  negate = Vector.map negate

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
            UV.unsafeIndex ys (n Num.- k)) zero [kmin .. kmax]
          where
            !kmin = max 0 (n Num.- (klen Num.- 1))
            !kmax = min n (slen Num.- 1)
        !slen = UV.length xs
        !klen = UV.length ys
        !maxlen = max slen klen

instance (UV.Unbox a, Ring a) => Ring (UV.Vector a) where
  negate = UV.map negate

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
            SV.unsafeIndex ys (n Num.- k)) zero [kmin .. kmax]
          where
            !kmin = max 0 (n Num.- (klen Num.- 1))
            !kmax = min n (slen Num.- 1)
        !slen = SV.length xs
        !klen = SV.length ys
        !maxlen = max slen klen

instance (SV.Storable a, Ring a) => Ring (SV.Vector a) where
  negate = SV.map negate
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
