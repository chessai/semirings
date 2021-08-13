{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module: Data.Ring.Ordered
-- Copyright: (C) 2021 Koz Ross
-- License: 
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- An \'ordered ring\' is a ring with a total order.
--
-- = Mathematical pedantry note
--
-- Many (if not most) of the instances of the 'OrderedRing' type class are not
-- truly ordered rings in the mathematical sense, as the
-- [axioms](https://en.wikipedia.org/wiki/Ordered_ring) imply that the
-- underlying set is either a singleton or infinite. Thus, the [additional
-- properties](https://en.wikipedia.org/wiki/Ordered_ring#Basic_properties) of
-- ordered rings do not, in general, hold. The only exception is that for any
-- @OrderedRing a => x :: a@, @x@ is either positive (that is, @'signum' x ==
-- 'Positive'@), negative (that is, @'signum' x == 'Negative'@) or @zero@.
--
-- We indicate those instances that /are/ \'truly\' or \'mathematically\'
-- ordered rings in their documentation.
module Data.Ring.Ordered 
  (
    -- * Helper type
    Signum(..),
    -- * Ordered ring type class
    OrderedRing(..)
  ) where

import Control.Applicative (Const (Const))
#if MIN_VERSION_base(4,7,0)
import Data.Data (Data)
#endif
import Data.Fixed (HasResolution, Fixed)
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ord (Down (Down))
import Data.Ratio (Ratio)
import Data.Semiring (Ring (negate), zero)
import Data.Monoid (Dual (Dual))
import GHC.Generics (Generic)
import Prelude hiding (signum, abs, negate, (-))
import qualified Prelude as Num
#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable)
#endif

-- | A pattern-matching-friendly representation of the signum of a value.
--
-- @since 0.7
data Signum = Negative | Zero | Positive
  deriving
    ( Bounded -- ^ @since 0.7
    , Eq -- ^ @since 0.7
    , Ord -- ^ @since 0.7
    , Show -- ^ @since 0.7
    , Read -- ^ @since 0.7
    , Generic -- ^ @since 0.7
#if MIN_VERSION_base(4,7,0)
    , Data -- ^ @since 0.7
    , Typeable -- ^ @since 0.7
#endif
    )

-- @since 0.7
instance Semigroup Signum where
  Zero <> _ = Zero
  _ <> Zero = Zero
  Positive <> Positive = Positive
  Negative <> Negative = Positive
  _ <> _ = Negative

-- @since 0.7
instance Monoid Signum where
  mempty = Positive

-- | The class of rings which also have a total order.
--
-- Instance should satisfy the following laws:
--
-- * @'abs' 'zero' = 'zero'@
-- * @'abs' x = 'abs' ('negate' x)@
-- * @x 'Data.Semiring.-' 'abs' x = 'zero'@
-- * @'signum' 'zero' = 'Zero'@
-- * If @x '>' 'zero'@, then @'signum' x = 'Positive'@
-- * If @x '<' 'zero'@, then @'signum' x = 'Negative'@
--
-- Additionally, if you define both 'signum' and 'abs', the implementations 
-- are subject to the following consistency laws:
--
-- * @'signum' x = 'Negative'@ if and only if @'abs' x /= x@
--
-- @since 0.7
class (Ring a, Ord a) => OrderedRing a where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL abs | signum #-}
#endif
  -- | Compute the absolute value.
  abs :: a -> a
  abs x = case signum x of 
    Negative -> negate x
    _ -> x
  -- | Determine the \'sign\' of a value.
  signum :: a -> Signum
  signum x 
    | x == zero = Zero
    | x == abs x = Positive
    | otherwise = Negative

-- | This instance is a \'true\' or \'mathematical\' ordered ring, as it is a
-- singleton. We assume that '()' has a zero signum.
--
-- @since 0.7
instance OrderedRing () where
  abs = const ()
  signum = const Zero

-- | @since 0.7
instance (OrderedRing a) => OrderedRing (Dual a) where
  abs (Dual x) = Dual . abs $ x
  signum (Dual x) = signum x

-- | @since 0.7
instance (OrderedRing a) => OrderedRing (Const a b) where
  abs (Const x) = Const . abs $ x
  signum (Const x) = signum x

-- | Where @a ~ 'Integer'@, this instance is a \'true\' or \'mathematical\'
-- ordered ring, as the resulting type is infinite.
--
-- @since 0.7
instance (Integral a) => OrderedRing (Ratio a) where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | @since 0.7
deriving instance (OrderedRing a) => OrderedRing (Down a)

-- | @since 0.7
deriving instance (OrderedRing a) => OrderedRing (Identity a)

-- | @since 0.7
instance (HasResolution a) => OrderedRing (Fixed a) where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | @since 0.7
instance OrderedRing Int8 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | @since 0.7
instance OrderedRing Int16 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | @since 0.7
instance OrderedRing Int32 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | @since 0.7
instance OrderedRing Int64 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | @since 0.7
instance OrderedRing Int where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- | This instance is a \'true\' or \'mathematical\' ordered ring, as 'Integer'
-- is an infinite type.
--
-- @since 0.7
instance OrderedRing Integer where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive

-- @since 0.7
instance (Ring (a, b), OrderedRing a, OrderedRing b) => OrderedRing (a, b) where
  abs (x, y) = (abs x, abs y)
  signum (x, y) = signum x <> signum y
