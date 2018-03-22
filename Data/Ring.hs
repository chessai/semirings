{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}
-- this is here because of Complex
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Ring
  ( Ring(..)
  , minus
  , (-)
  ) where

import           Control.Applicative (Alternative(..), Applicative(..), Const(..))
import           Data.Complex (Complex(..))
import           Data.Fixed (Fixed, HasResolution)
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int, Int8, Int16, Int32, Int64)
--import           Data.Maybe
import           Data.Monoid
import           Data.Ord (Down(..))
import           Data.Ratio (Ratio)
import           Data.Semigroup (Min(..), Max(..))
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Foreign.C.Types
  (CChar, CClock, CDouble, CFloat, CInt,
   CIntMax, CIntPtr, CLLong, CLong,
   CPtrdiff, CSChar, CSUSeconds, CShort,
   CSigAtomic, CSize, CTime, CUChar, CUInt,
   CUIntMax, CUIntPtr, CULLong, CULong,
   CUSeconds, CUShort, CWchar)
import           Foreign.Ptr (IntPtr, WordPtr)
import           Numeric.Natural (Natural)
import qualified Prelude as P
import           Prelude (Integral, Integer, Float, Double)
import           System.Posix.Types
  (CCc, CDev, CGid, CIno, CMode, CNlink,
   COff, CPid, CRLim, CSpeed, CSsize,
   CTcflag, CUid, Fd)

import           Data.Semiring (Semiring(..), (+), (*), WrappedApplicative(..))

infixl 6 -

(-), minus :: Ring a => a -> a -> a
minus x y = x `plus` (negate y)
(-) = minus

class Semiring a => Ring a where
  {-# MINIMAL negate #-} 
  negate :: a -> a

  default negate :: P.Num a => a -> a
  negate = P.negate

-- This is an orphan and I hate that.
instance Ring a => Semiring (Complex a) where
  zero = zero :+ zero
  one  = one  :+ zero
  plus  (x :+ y) (x' :+ y') = (plus x x') :+ (plus y y')
  times (x :+ y) (x' :+ y')
    = (x * x' + (negate (y * y'))) :+ (x * y' + y * x')

instance Ring a => Ring (Complex a) where
  negate (x :+ y) = negate x :+ negate y

instance Ring b => Ring (a -> b) where
  negate f x = negate (f x)

-- This is not a true ring unless the underlying
-- type is an abelian group, and it is non-commutative.
deriving instance Ring a => Ring (Endo a)

instance (Applicative f, Ring a) => Ring (WrappedApplicative f a) where
  negate = P.fmap negate

instance (Alternative f, Ring a) => Ring (Alt f a) where
  negate = P.fmap negate

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
deriving instance Ring a => Ring (Down a)
deriving instance Ring a => Ring (Product a)
deriving instance Ring a => Ring (Sum a)
deriving instance Ring a => Ring (Identity a)
deriving instance Ring a => Ring (Const a b)
deriving instance Ring a => Ring (Max a)
deriving instance Ring a => Ring (Min a)
instance HasResolution a => Ring (Fixed a)
