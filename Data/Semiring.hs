{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Semiring 
  ( Semiring(..)
  , Poly(..)
  , Poly2(..)
  , Poly3(..)
  , (+)
  , (*)
  , (+++)
  , (***)
  , semisum
  , semiprod
  , semisum'
  , semiprod'
  ) where

import           Control.Applicative (Applicative(..), Const(..))
import           Data.Bool (Bool(..), (||), (&&))
import           Data.Complex (Complex(..))
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Int (Int, Int8, Int16, Int32, Int64)
--import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           GHC.Generics (Generic, Generic1)
import qualified Prelude as P
import           Prelude (IO, ($))

infixl 7 *, ***, `times`
infixl 6 +, +++, `plus`

(+), (*) :: Semiring a => a -> a -> a
(+) = plus
(*) = times

(+++), (***) :: Semiring a => a -> a -> a
(+++) = plus
(***) = times

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

class Semiring a => Ring a where
  {-# MINIMAL negate #-} 
  negate :: a -> a

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

  plus  = listPlus 
  times = listTimes

listPlus :: Semiring a => [a] -> [a] -> [a]
listPlus [] y = y
listPlus x [] = x
listPlus xs ys = liftA2 plus xs ys

listTimes :: Semiring a => [a] -> [a] -> [a]
listTimes [] _ = []
listTimes _ [] = []
listTimes xs ys = liftA2 times xs ys

instance Ring a => Semiring (Complex a) where
  zero = zero :+ zero
  one  = one  :+ zero

  plus  (x :+ y) (x' :+ y') = (plus x x') :+ (plus y y')
  times (x :+ y) (x' :+ y')
    = (x * x' + (negate (y * y'))) :+ (x * y' + y * x')

instance Semiring a => Semiring (Vector a) where
  zero = Vector.empty
  one  = Vector.singleton one

  plus  = vectorPlus
  times = vectorTimes

instance (Semiring a, Semiring b) => Semiring (a,b) where
  zero = (zero,zero)
  one = (one,one)
  (a1,b1) `plus` (a2,b2) =
    (a1 `plus` a2, b1 `plus` b2)
  (a1,b1) `times` (a2,b2) =
    (a1 `times` a2, b1 `times` b2)

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
  zero = (zero, zero, zero)
  one = (one,one,one)
  (a1,b1,c1) `plus` (a2,b2,c2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2)
  (a1,b1,c1) `times` (a2,b2,c2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2)

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
  zero = (zero, zero, zero, zero)
  one = (one, one, one, one)
  (a1,b1,c1,d1) `plus` (a2,b2,c2,d2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2)
  (a1,b1,c1,d1) `times` (a2,b2,c2,d2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a,b,c,d,e) where
  zero = (zero, zero, zero, zero, zero)
  one = (one, one, one, one, one)
  (a1,b1,c1,d1,e1) `plus` (a2,b2,c2,d2,e2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2, e1 `plus` e2)
  (a1,b1,c1,d1,e1) `times` (a2,b2,c2,d2,e2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2, e1 `times` e2)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f) => Semiring (a,b,c,d,e,f) where
  zero = (zero, zero, zero, zero, zero, zero)
  one  = (one, one, one, one, one, one)
  (a1,b1,c1,d1,e1,f1) `plus` (a2,b2,c2,d2,e2,f2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2, e1 `plus` e2, f1 `plus` f2)
  (a1,b1,c1,d1,e1,f1) `times` (a2,b2,c2,d2,e2,f2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2, e1 `times` e2, f1 `times` f2)
 
instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g) => Semiring (a,b,c,d,e,f,g) where
  zero = (zero, zero, zero, zero, zero, zero, zero)
  one  = (one, one, one, one, one, one, one)
  (a1,b1,c1,d1,e1,f1,g1) `plus` (a2,b2,c2,d2,e2,f2,g2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2, e1 `plus` e2, f1 `plus` f2, g1 `plus` g2)
  (a1,b1,c1,d1,e1,f1,g1) `times` (a2,b2,c2,d2,e2,f2,g2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2, e1 `times` e2, f1 `times` f2, g1 `times` g2)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g, Semiring h) => Semiring (a,b,c,d,e,f,g,h) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero)
  one  = (one, one, one, one, one, one, one, one)
  (a1,b1,c1,d1,e1,f1,g1,h1) `plus` (a2,b2,c2,d2,e2,f2,g2,h2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2, e1 `plus` e2, f1 `plus` f2, g1 `plus` g2, h1 `plus` h2)
  (a1,b1,c1,d1,e1,f1,g1,h1) `times` (a2,b2,c2,d2,e2,f2,g2,h2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2, e1 `times` e2, f1 `times` f2, g1 `times` g2,h1 `times` h2)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g, Semiring h, Semiring i) => Semiring (a,b,c,d,e,f,g,h,i) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero)
  one  = (one, one, one, one, one, one, one, one, one)
  (a1,b1,c1,d1,e1,f1,g1,h1,i1) `plus` (a2,b2,c2,d2,e2,f2,g2,h2,i2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2, e1 `plus` e2, f1 `plus` f2, g1 `plus` g2, h1 `plus` h2, i1 `plus` i2)
  (a1,b1,c1,d1,e1,f1,g1,h1,i1) `times` (a2,b2,c2,d2,e2,f2,g2,h2,i2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2, e1 `times` e2, f1 `times` f2, g1 `times` g2,h1 `times` h2, i1 `times` i2)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g, Semiring h, Semiring i, Semiring j) => Semiring (a,b,c,d,e,f,g,h,i,j) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero, zero)
  one  = (one, one, one, one, one, one, one, one, one, one)
  (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) `plus` (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) =
    (a1 `plus` a2, b1 `plus` b2, c1 `plus` c2, d1 `plus` d2, e1 `plus` e2, f1 `plus` f2, g1 `plus` g2, h1 `plus` h2, i1 `plus` i2, j1 `plus` j2)
  (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) `times` (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) =
    (a1 `times` a2, b1 `times` b2, c1 `times` c2, d1 `times` d2, e1 `times` e2, f1 `times` f2, g1 `times` g2,h1 `times` h2, i1 `times` i2, j1 `times` j2)

instance Semiring Bool where
  plus  = (||)
  zero  = False
  times = (&&)
  one   = True

instance Semiring Int where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Int8 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Int16 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Int32 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Int64 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Word where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Word8 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Word16 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Word32 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Word64 where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

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

deriving newtype instance Semiring a => Semiring (Endo a)

-- this should be wrapped applicative
instance (Applicative f, Semiring a) => Semiring (Alt f a) where
  zero  = Alt (pure zero)
  one   = Alt (pure one)
  plus  = liftA2 plus
  times = liftA2 times

instance Semiring a => Semiring (Const a b) where
  zero = Const zero
  one  = Const one
  plus  (Const x) (Const y) = Const (x `plus`  y)
  times (Const x) (Const y) = Const (x `times` y)

instance (P.Ord a, Semiring a) => Semiring (Set a) where
  zero  = Set.empty
  one   = Set.singleton one
  plus  = Set.union
  times xs ys = Set.map (P.uncurry times) (Set.cartesianProduct xs ys)

-- | The type of polynomials in one variable
newtype Poly  a   = Poly  (Vector a)
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor, P.Applicative, P.Monad)

-- | The type of polynomials in two variables
newtype Poly2 a b = Poly2 (Vector (a,b))
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

-- | The type of polynomials in three variables
newtype Poly3 a b c = Poly3 (Vector (a,b,c))
  deriving (P.Eq, P.Ord, P.Read, P.Show, Generic, Generic1,
            P.Functor)

vectorPlus :: Semiring a => Vector a -> Vector a -> Vector a
vectorPlus x y
  = if Vector.null x then y else if Vector.null y then x else
  liftA2 plus x y

vectorTimes :: Semiring a => Vector a -> Vector a -> Vector a
vectorTimes x y
  = if Vector.null x then x else if Vector.null y then y else
  liftA2 times x y

-- | There is no way to do better than /O(n^2)/ for any
-- semiring. There is an algorithm that is /O(n log n)/,
-- but it requires something at least as strong as 'RealFloat'.
polyTimes :: Semiring a => Vector a -> Vector a -> Vector a
polyTimes x y
  = if Vector.null x then x else if Vector.null y then y else
      Vector.cons (times a b) (Vector.map (a *) q + Vector.map (* b) p + (Vector.cons zero (polyTimes p q)))
  where
    a = Vector.unsafeHead x
    b = Vector.unsafeHead y
    p = (\t d -> if Vector.null t then d else t) (Vector.tail x) (Vector.empty)
    q = (\t d -> if Vector.null t then d else t) (Vector.tail y) (Vector.empty)

instance Semiring a => Semiring (Poly a) where
  zero = Poly (Vector.empty)
  one  = Poly (Vector.singleton one)

  plus  (Poly x) (Poly y) = Poly $ vectorPlus  x y
  times (Poly x) (Poly y) = Poly $ polyTimes x y

instance (Semiring a, Semiring b) => Semiring (Poly2 a b) where
  zero = Poly2 (Vector.empty)
  one  = Poly2 (Vector.singleton one)

  plus  (Poly2 x) (Poly2 y) = Poly2 $ vectorPlus  x y
  times (Poly2 x) (Poly2 y) = Poly2 $ polyTimes x y

instance (Semiring a, Semiring b, Semiring c) => Semiring (Poly3 a b c) where
  zero = Poly3 (Vector.empty)
  one  = Poly3 (Vector.singleton one)

  plus  (Poly3 x) (Poly3 y) = Poly3 $ vectorPlus  x y
  times (Poly3 x) (Poly3 y) = Poly3 $ polyTimes x y
