{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semiring.Free
  ( Free(..)
  , runFree
  , lowerFree
  , liftFree
  ) where

import           Control.Applicative (pure)
import           Data.Bool (otherwise)
import           Data.Coerce (Coercible, coerce)
import           Data.Eq (Eq)
import           Data.Functor (Functor(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Function (flip,id, (.))
import           Data.Ord (Ord)
import           Data.Semiring
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Monoid(..))
import           Data.Sequence (Seq)

import           GHC.Show (Show)
import           GHC.Read (Read)
import           GHC.Real (even, div)
import           Numeric.Natural

newtype Free a = Free
  { getFree :: Map (Identity a) Natural
  } deriving (Show, Read, Eq, Ord, Semiring)

runFree :: Semiring s => (a -> s) -> Free a -> s
runFree f = getAdd #. Map.foldMapWithKey ((rep .# Add) . prod . fmap f) . getFree

lowerFree :: Semiring s => Free s -> s
lowerFree = runFree id

liftFree :: a -> Free a
liftFree = Free . flip Map.singleton one . pure

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
