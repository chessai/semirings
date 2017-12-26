{-# OPTIONS_GHC -Wall #-}

module Data.Semiring 
  ( Semiring(..)
  ) where

import           Data.Bool (Bool(..), (||), (&&))
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Prelude as P
import           Prelude (Double, Float)

class Semiring a where
  {-# MINIMAL plus, zero, times, one #-}
  plus  :: a -> a -> a -- ^ Additive Magma
  zero  :: a           -- ^ Additive Unital
  times :: a -> a -> a -- ^ Multiplicative Magma
  one   :: a           -- ^ Multiplicative Unital

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

instance Semiring Double where
  plus  = (P.+)
  zero  = 0
  times = (P.*)
  one   = 1

instance Semiring Float where
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


