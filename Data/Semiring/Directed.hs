{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-----------------------------------------------------------------------------
-- |
--   A "directed semiring" refers to the semiring composed of the union of upwards
--   directed sets as multiplication, and intersection of downwards directed sets
--   as addition.
-----------------------------------------------------------------------------
module Data.Semiring.Directed
  ( -- * Directed semirings
    Directed(..)
  , Above(..)
  , Below(..)
  ) where

#if MIN_VERSION_base(4,7,0)
import Data.Data (Data)
#endif
import Data.Coerce (coerce)
import Data.Semiring (Semiring(..))
#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable)
#endif

import GHC.Generics (Generic)


-- | As it is above.
--
-- 'Above' provides the monoid associated with the union of upward-directed sets.
--
-- @since 0.7
newtype Above = Above { 
  -- | @since 0.7
  getAbove :: Ordering 
  }
  deriving
    ( Bounded -- ^ @since 0.7
    , Enum -- ^ @since 0.7
    , Eq -- ^ @since 0.7
    , Generic  -- ^ @since 0.7
    , Show -- ^ @since 0.7
    , Read -- ^ @since 0.7
#if MIN_VERSION_base(4,7,0)
    , Data -- ^ @since 0.7
    , Typeable -- ^ @since 0.7
#endif
    )

-- | @since 0.7
instance Semigroup Above where
  Above LT <> a = a
  a <> Above LT = a
  Above EQ <> Above EQ = Above EQ
  _ <> Above GT = Above GT
  Above GT <> _ = Above GT

-- | @since 0.7
instance Monoid Above where
  mempty = Above LT

-- | So it shall be below.
--
-- 'Below' provides the monoid associated with the intersection of downward-directed sets.
--
-- @since 0.7
newtype Below = Below { 
  -- | @since 0.7
  getBelow :: Ordering 
  }
  deriving
    ( Bounded -- ^ @since 0.7
    , Enum -- ^ @since 0.7
    , Eq -- ^ @since 0.7
    , Generic  -- ^ @since 0.7
    , Show -- ^ @since 0.7
    , Read -- ^ @since 0.7
#if MIN_VERSION_base(4,7,0)
    , Data -- ^ @since 0.7
    , Typeable -- ^ @since 0.7
#endif
    )

-- | @since 0.7
instance Semigroup Below where
  Below GT <> a = a
  a <> Below GT = a
  Below EQ <> Below EQ = Below EQ
  Below EQ <> _ = Below LT
  _ <> Below EQ = Below LT

-- | @since 0.7
instance Monoid Below where
  mempty = Below GT

-- | Wrapper for the semiring of upwards and downwards directed sets.
--
-- For the individual join/meet monoids associated with either
-- algebra, see 'Above', and 'Below'.
newtype Directed = Directed { 
  -- | @since 0.7
  getDirected :: Ordering 
  }
  deriving
    ( Bounded -- ^ @since 0.7
    , Enum -- ^ @since 0.7
    , Eq -- ^ @since 0.7
    , Generic  -- ^ @since 0.7
    , Show -- ^ @since 0.7
    , Read -- ^ @since 0.7
#if MIN_VERSION_base(4,7,0)
    , Data -- ^ @since 0.7
    , Typeable -- ^ @since 0.7
#endif
    )

-- | @since 0.7
instance Semiring Directed where
  plus = coerce ((<>) :: Above -> Above -> Above)
  zero = coerce (mempty :: Above)
  times = coerce ((<>) :: Below -> Below -> Below)
  one = coerce (mempty :: Below)
