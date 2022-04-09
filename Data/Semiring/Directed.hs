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
  ) where

import Data.Data (Data)
import Data.Coerce (coerce)
import Data.Semiring (Semiring(..))
import Data.Semigroup (Min(Min), Max(Max), (<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Wrapper for the semiring of upwards and downwards directed sets.
--
-- For the individual join/meet monoids associated with either
-- algebra, see @'Max' 'Ordering', and @'Min' 'Ordering'@.
newtype Directed = Directed {
  -- | @since 0.7
  getDirected :: Ordering
  }
  deriving (
    -- | @since 0.7
    Bounded,
    -- | @since 0.7
    Enum,
    -- | @since 0.7
    Eq,
    -- | @since 0.7
    Generic,
    -- | @since 0.7
    Show,
    -- | @since 0.7
    Read,
    -- | @since 0.7
    Data,
    -- | @since 0.7
    Typeable
    )

-- | @since 0.7
instance Semiring Directed where
  plus = coerce ((<>) :: Max Ordering -> Max Ordering -> Max Ordering)
  zero = coerce (mempty :: Max Ordering)
  times = coerce ((<>) :: Min Ordering -> Min Ordering -> Min Ordering)
  one = coerce (mempty :: Min Ordering)
