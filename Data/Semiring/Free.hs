{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

#if !MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

-- | Polynomials with natural number coefficients
--   form a commutative semiring. In fact, this is
--   the free commutative semiring.
module Data.Semiring.Free
  (
#if defined(VERSION_containers)
#if MIN_VERSION_base(4,8,0)
    Free(..)
  , runFree
  , lowerFree
  , liftFree
#endif
#endif
  ) where

#if defined(VERSION_containers)
#if MIN_VERSION_base(4,8,0)
import           Data.Bool (otherwise)
import           Data.Coerce (Coercible, coerce)
import           Data.Eq (Eq)
import           Data.Functor.Identity (Identity(..))
import           Data.Function (flip,id, (.))
import           Data.Ord (Ord)
#if !MIN_VERSION_base(4,9,0)
import           Data.Semigroup ()
#endif
import           Data.Semiring
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Monoid(..))

import           GHC.Show (Show)
import           GHC.Read (Read)
import           GHC.Real (even, div)
import           Numeric.Natural

-- | N[x], polynomials with natural number
--   coefficients form the free commutative semiring
--   on a single generator {x}.
newtype Free a = Free
  { getFree :: Map a Natural
  } deriving (Show, Read, Eq, Ord, Semiring)

#if !MIN_VERSION_base(4,9,0)
deriving instance Monoid a => Monoid (Identity a)
#endif

-- | Run a 'Free'
runFree :: Semiring s => (a -> s) -> Free a -> s
runFree f = getAdd #.
  Map.foldMapWithKey
  ((rep .# Add) . product . Identity . f)
  . getFree
{-# INLINE runFree #-}

-- | Run a 'Free', interpreting it in the underlying semiring.
lowerFree :: Semiring s => Free s -> s
lowerFree = runFree id
{-# INLINE lowerFree #-}

-- | Create a 'Free' with one item.
liftFree :: a -> Free a
liftFree = Free . flip Map.singleton one
{-# INLINE liftFree #-}

rep :: Monoid m => m -> Natural -> m
rep x = go
  where
    go 0 = mempty
    go 1 = x
    go n
      | even n = r `mappend` r
      | otherwise = x `mappend` r `mappend` r
      where
        r = go (n `div` 2)
{-# INLINE rep #-}

-- | Coercive left-composition.
infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | Coercive right-composition.
infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}

#endif

#endif
