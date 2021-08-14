{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- We need an orphan instance of Semiring for Complex, as it requires a Ring
-- superclass to work.
{-# OPTIONS_GHC -Wno-orphans            #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE DerivingVia                #-}
#else
{-# LANGUAGE TemplateHaskell            #-}
#endif

-- |
-- Module: Data.Ring
-- Copyright: (C) chessai 2018, (C) 2021 Koz Ross
-- License: BSD3 
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- A ring is a semiring with an additive inverse.
module Data.Ring (
  -- * Ring type class
  Ring(..),
  -- * Functions
  (-),
  minus,
  fromInteger,
  fromIntegral
  ) where

import Control.Applicative (Const (Const))
import Data.Complex (Complex((:+)))
import Data.Fixed (Fixed, HasResolution)
#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant (Op(Op))
#endif
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (Dual (Dual))
#if MIN_VERSION_base(4,12,0)
import Data.Monoid (Ap)
#endif
import Data.Ord (Down (Down))
import Data.Ratio (Ratio)
import Data.Semiring (Semiring (times, plus, zero, one, fromNatural), 
                      WrappedNum (WrapNum), Mod2, (*), (+))
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types
  (CChar, CClock, CDouble, CFloat, CInt,
   CIntMax, CIntPtr, CLLong, CLong,
   CPtrdiff, CSChar, CSUSeconds, CShort,
   CSigAtomic, CSize, CTime, CUChar, CUInt,
   CUIntMax, CUIntPtr, CULLong, CULong,
   CUSeconds, CUShort, CWchar)
import Foreign.Ptr (IntPtr, WordPtr)
import qualified GHC.Num as Num
import qualified GHC.Real as Real
import Prelude hiding ((-), negate, fromInteger, fromIntegral, (+), (*))

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
import           System.Posix.Types
  (CCc, CDev, CGid, CIno, CMode, CNlink,
   COff, CPid, CRLim, CSpeed, CSsize,
   CTcflag, CUid, Fd)
#endif

-- | The class of semirings with an additive inverse.
--
--     @'negate' a '+' a = 'zero'@
--
-- @since 0.1.0
class Semiring a => Ring a where
  {-# MINIMAL negate #-}
  negate :: a -> a

-- | @since 0.2.1.1
instance Num.Num a => Ring (WrappedNum a) where
  negate = Num.negate

-- | @since 0.4.2
instance Ring Mod2 where
  negate = id
  {-# INLINE negate #-}

-- | @since 0.1.0
instance Ring b => Ring (a -> b) where
  negate f x = negate (f x)
  {-# INLINE negate #-}

-- | @since 0.1.0
instance Ring () where
  negate _ = ()
  {-# INLINE negate #-}

-- | @since 0.1.0
instance Ring a => Ring (IO a) where
  negate = fmap negate
  {-# INLINE negate #-}

-- | @since 0.1.0
instance Ring a => Ring (Dual a) where
  negate (Dual x) = Dual (negate x)
  {-# INLINE negate #-}

-- | @since 0.1.0
instance Ring a => Ring (Const a b) where
  negate (Const x) = Const (negate x)
  {-# INLINE negate #-}

#if MIN_VERSION_base(4,12,0)
-- | @since 0.1.0
instance (Ring a, Applicative f) => Ring (Ap f a) where
  negate = fmap negate
  {-# INLINE negate #-}

-- | @since 0.2.0.1
deriving instance Ring a => Ring (Op a b)
#endif

instance Integral a => Ring (Ratio a) where
  negate = Num.negate
  {-# INLINE negate #-}

-- | @since 0.1.0
deriving instance Ring a => Ring (Down a)

-- | @since 0.1.0
deriving instance Ring a => Ring (Identity a)

-- | @since 0.1.0
instance HasResolution a => Ring (Fixed a) where
  negate = Num.negate
  {-# INLINE negate #-}

-- | This instance can suffer due to floating point arithmetic.
--
-- @since 0.1.0
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

-- | @since 0.1.0
instance Ring a => Ring (Complex a) where
  negate (x :+ y) = negate x :+ negate y
  {-# INLINE negate #-}

-- | Infix shorthand for 'minus'.
--
-- @since 0.1.0
(-) :: Ring a => a -> a -> a
(-) = minus
{-# INLINE (-) #-}

infixl 6 -, `minus`

-- | Subtract two 'Ring' values. For any type @R@ with
-- a 'Num.Num' instance, this is the same as '(Prelude.-)'.
--
--     @x `minus` y = x '+' 'negate' y@
--
-- @since 0.1.0
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
--
-- @since 0.5
fromInteger :: Ring a => Integer -> a
fromInteger x
  | x >= 0    = fromNatural (Num.fromInteger x)
  | otherwise = negate (fromNatural (Num.fromInteger (Num.negate x)))
{-# INLINE fromInteger #-}

-- | Convert from integral to ring.
--
-- @since 0.5
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral x
  | x >= 0    = fromNatural (Real.fromIntegral x)
  | otherwise = negate (fromNatural (Real.fromIntegral (Num.negate x)))
{-# INLINE fromIntegral #-}

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
deriving via (WrappedNum CCc) instance Ring CCc
deriving via (WrappedNum CDev) instance Ring CDev
deriving via (WrappedNum CGid) instance Ring CGid
deriving via (WrappedNum CIno) instance Ring CIno
deriving via (WrappedNum CMode) instance Ring CMode
deriving via (WrappedNum CNlink) instance Ring CNlink
deriving via (WrappedNum COff) instance Ring COff
deriving via (WrappedNum CPid) instance Ring CPid
deriving via (WrappedNum CRLim) instance Ring CRLim
deriving via (WrappedNum CSpeed) instance Ring CSpeed
deriving via (WrappedNum CSsize) instance Ring CSsize
deriving via (WrappedNum CTcflag) instance Ring CTcflag
deriving via (WrappedNum CUid) instance Ring CUid
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
