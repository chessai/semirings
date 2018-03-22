{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

--{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

module Data.Ring
  ( Ring(..)
  , minus
  , (-)
  ) where

import           Control.Applicative (Applicative(..), Const(..))
import           Data.Bool (Bool(..), (||), (&&))
import           Data.Complex (Complex(..))
import           Data.Fixed (Fixed, HasResolution)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int, Int8, Int16, Int32, Int64)
--import           Data.Maybe
import           Data.Monoid
import           Data.Ratio (Ratio)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
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
import           Prelude (IO, Integral, Integer, Float, Double)
import           System.Posix.Types
  (CCc, CDev, CGid, CIno, CMode, CNlink,
   COff, CPid, CRLim, CSpeed, CSsize,
   CTcflag, CUid, Fd)

import           Data.Semiring (Semiring(..), (+), (*))

class Semiring a => Ring a where
  {-# MINIMAL negate #-} 
  negate :: a -> a

  default negate :: P.Num a => a -> a
  negate = P.negate

infixl 6 -

(-), minus :: Ring a => a -> a -> a
minus x y = x `plus` (negate y)

(-) = minus

instance Ring a => Semiring (Complex a) where
  zero = zero :+ zero
  one  = one  :+ zero
  plus  (x :+ y) (x' :+ y') = (plus x x') :+ (plus y y')
  times (x :+ y) (x' :+ y')
    = (x * x' + (negate (y * y'))) :+ (x * y' + y * x')
