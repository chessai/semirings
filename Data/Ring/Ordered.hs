{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Trustworthy                #-}

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
    -- * Helper types
    Signum(..), 
    Modular(..),
    -- * Ordered ring type class
    OrderedRing(..),
  ) where

import Control.Applicative (Const (Const))
import Data.Data (Data)
import Data.Fixed (HasResolution, Fixed)
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (Dual (Dual))
import Data.Ord (Down (Down))
import Data.Ratio (Ratio)
import Data.Semiring (Ring (negate), Semiring(zero, one))
import Data.Semiring.Generic ()
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Prelude hiding (signum, abs, negate, (-))
import qualified Prelude as Num
import Data.Typeable (Typeable)

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
    , Data -- ^ @since 0.7
    , Typeable -- ^ @since 0.7
    )

-- | If we treat 'Negative' as @-1@, 'Zero' as @0@ and 'Positive' as @1@, we 
-- get a 'Semigroup' under multiplication.
--
-- @since 0.7
instance Semigroup Signum where
  Zero <> _ = Zero
  _ <> Zero = Zero
  Positive <> Positive = Positive
  Negative <> Negative = Positive
  _ <> _ = Negative

-- | Under the mapping described in the 'Semigroup' instance, 'One' is the
-- multiplicative identity.
--
-- @since 0.7
instance Monoid Signum where
  mempty = Positive

-- | A wrapper to indicate the type is being treated as a [modular arithmetic
-- system](https://en.wikipedia.org/wiki/Modular_arithmetic) whose modulus is
-- the type's cardinality.
--
-- While we cannot guarantee that infinite types won't be wrapped by this, we
-- only provide instances of the relevant type classes for those types we are
-- certain are finite.
--
-- @since 0.7
newtype Modular a = Modular { getModular :: a }
  deriving
    ( Bounded -- ^ @since 0.7
    , Eq -- ^ @since 0.7
    , Ord -- ^ @since 0.7
    , Show -- ^ @since 0.7
    , Read -- ^ @since 0.7
    , Generic -- ^ @since 0.7
    , Data -- ^ @since 0.7
    , Typeable -- ^ @since 0.7
    )

-- @since 0.7
deriving instance Semiring (Modular Word8)

-- @since 0.7
deriving instance Semiring (Modular Word16)

-- @since 0.7
deriving instance Semiring (Modular Word32)

-- @since 0.7
deriving instance Semiring (Modular Word64)

-- @since 0.7
deriving instance Semiring (Modular Word)

-- @since 0.7
deriving instance Ring (Modular Word8)

-- @since 0.7
deriving instance Ring (Modular Word16)

-- @since 0.7
deriving instance Ring (Modular Word32)

-- @since 0.7
deriving instance Ring (Modular Word64)

-- @since 0.7
deriving instance Ring (Modular Word)

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
-- * @'signum\'' 'zero' = 'zero'@
-- * If @x '>' 'zero'@, then @'signum\'' x = 'one'@
-- * If @x '<' 'zero'@, then @'signum\'' x = 'negate' 'one'@
--
-- Additionally, if you define several methods, the implementations 
-- are subject to the following consistency laws:
--
-- * @'signum' x = 'Negative'@ if and only if @'abs' x /= x@
-- * @'signum\'' x = 'negate' 'one' if and only if @'abs' x /= x@
--
-- @since 0.7
class (Ring a, Ord a) => OrderedRing a where
  {-# MINIMAL abs | signum  | signum' #-}
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
  -- | Determine the \'sign\' of a value as an element of @a@.
  --
  -- This is designed mostly as a compatibility hack with @base@. Prefer
  -- 'signum' where possible.
  signum' :: a -> a
  signum' x 
    | x == zero = zero
    | x == abs x = one
    | otherwise = negate one

-- | This instance is a \'true\' or \'mathematical\' ordered ring, as it is a
-- singleton. We assume that '()' has a zero signum.
--
-- @since 0.7
instance OrderedRing () where
  abs = const ()
  signum = const Zero
  signum' = const zero

-- | Where @a@ is a \'true\' or \'mathematical\' ordered ring, so is this.
--
-- @since 0.7
instance (OrderedRing a) => OrderedRing (Dual a) where
  abs (Dual x) = Dual . abs $ x
  signum (Dual x) = signum x
  signum' (Dual x) = Dual . signum' $ x

-- | Where @a@ is a \'true\' or \'mathematical\' ordered ring, so is this.
--
-- @since 0.7
instance (OrderedRing a) => OrderedRing (Const a b) where
  abs (Const x) = Const . abs $ x
  signum (Const x) = signum x
  signum' (Const x) = Const . signum' $ x

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
  signum' = Num.signum

-- | Where @a@ is a \'true\' or \'mathematical\' ordered ring, so is this.
--
-- @since 0.7
deriving instance (OrderedRing a) => OrderedRing (Down a)

-- | Where @a@ is a \'true\' or \'mathematical\' ordered ring, so is this.
--
-- @since 0.7
deriving instance (OrderedRing a) => OrderedRing (Identity a)

-- | @since 0.7
instance (HasResolution a) => OrderedRing (Fixed a) where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive
  signum' = Num.signum

-- | @since 0.7
instance OrderedRing Int8 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive
  signum' = Num.signum

-- | @since 0.7
instance OrderedRing Int16 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive
  signum' = Num.signum

-- | @since 0.7
instance OrderedRing Int32 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive
  signum' = Num.signum

-- | @since 0.7
instance OrderedRing Int64 where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive
  signum' = Num.signum

-- | @since 0.7
instance OrderedRing Int where
  abs = Num.abs
  signum x = case Num.signum x of 
    (-1) -> Negative
    0 -> Zero
    _ -> Positive
  signum' = Num.signum

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
  signum' = Num.signum

-- | @since 0.7
instance OrderedRing (Modular Word8) where
  abs x = x
  signum x 
    | x == zero = Zero
    | otherwise = Positive
  signum' (Modular x) = Modular . Num.signum $ x

-- | @since 0.7
instance OrderedRing (Modular Word16) where
  abs x = x
  signum x 
    | x == zero = Zero
    | otherwise = Positive
  signum' (Modular x) = Modular . Num.signum $ x

-- | @since 0.7
instance OrderedRing (Modular Word32) where
  abs x = x
  signum x 
    | x == zero = Zero
    | otherwise = Positive
  signum' (Modular x) = Modular . Num.signum $ x

-- | @since 0.7
instance OrderedRing (Modular Word64) where
  abs x = x
  signum x 
    | x == zero = Zero
    | otherwise = Positive
  signum' (Modular x) = Modular . Num.signum $ x

-- | @since 0.7
instance OrderedRing (Modular Word) where
  abs x = x
  signum x 
    | x == zero = Zero
    | otherwise = Positive
  signum' (Modular x) = Modular . Num.signum $ x

-- | Where @a@ and @b@ are both \'true\' or \'mathematical\' ordered rings, so is 
-- this.
--
-- @since 0.7
instance (OrderedRing a, OrderedRing b) => 
  OrderedRing (a, b) where
  abs (x, y) = (abs x, abs y)
  signum (x, y) = signum x <> signum y
  signum' (x, y) = (signum' x, signum' y)

-- | Where all of @a@, @b@, @c@ are \'true\' or \'mathematical\' ordered rings,
-- so is this.
--
-- @since 0.7
instance (OrderedRing a, OrderedRing b, OrderedRing c) =>
  OrderedRing (a, b, c) where
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = signum x <> signum y <> signum z
  signum' (x, y, z) = (signum' x, signum' y, signum' z)

-- | Where all of @a@, @b@, @c@, @d@ are \'true\' or \'mathematical\' ordered
-- rings, so is this.
--
-- @since 0.7
instance (OrderedRing a, OrderedRing b, OrderedRing c, OrderedRing d) =>
  OrderedRing (a, b, c, d) where
  abs (x, y, z, z2) = (abs x, abs y, abs z, abs z2)
  signum (x, y, z, z2) = signum x <> signum y <> signum z <> signum z2
  signum' (x, y, z, z2) = (signum' x, signum' y, signum' z, signum' z2)
