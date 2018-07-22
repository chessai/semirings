{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Semiring.Free
  (
  ) where

import           Data.Semiring

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Numeric.Natural

--newtype Free a = Free
--  { getFree :: Map (Seq a) Natural
--  } deriving (Show, Read, Eq, Ord, Semiring)


