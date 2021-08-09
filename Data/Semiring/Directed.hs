{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
-- @Above@ provides the monoid associated with the union of upward-directed sets.
newtype Above = Above { getAbove :: Ordering }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Generic
    , Show
    , Read
#if MIN_VERSION_base(4,7,0)
    , Data
    , Typeable
#endif
    )

instance Semigroup Above where
  Above LT <> a = a
  a <> Above LT = a
  Above EQ <> Above EQ = Above EQ
  Above EQ <> Above GT = Above GT
  Above GT <> Above EQ = Above GT
  Above GT <> Above GT = Above GT

instance Monoid Above where
  mempty = Above LT

-- | So it shall be below.
--
-- @Below@ provides the monoid associated with the intersection of downward-directed sets.
newtype Below = Below { getBelow :: Ordering }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Generic
    , Show
    , Read
#if MIN_VERSION_base(4,7,0)
    , Data
    , Typeable
#endif
    )

instance Semigroup Below where
  Below GT <> a = a
  a <> Below GT = a
  Below EQ <> Below EQ = Below EQ
  Below EQ <> Below LT = Below LT
  Below LT <> Below EQ = Below LT
  Below LT <> Below LT = Below LT

instance Monoid Below where
  mempty = Below GT

-- | Wrapper for the semiring of upwards and downwards directed sets.
--
-- For the individual join/meet monoids associated with either
-- algebra, see 'Above', and 'Below'.
newtype Directed = Directed { getDirected :: Ordering }
  deriving
    ( Bounded
    , Enum
    , Eq
    , Generic
    , Show
    , Read
#if MIN_VERSION_base(4,7,0)
    , Data
    , Typeable
#endif
    )

instance Semiring Directed where
  plus = coerce ((<>) :: Above -> Above -> Above)
  zero = coerce (mempty :: Above)
  times = coerce ((<>) :: Below -> Below -> Below)
  one = coerce (mempty :: Below)
