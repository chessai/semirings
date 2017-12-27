module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Semiring (Semiring(..))
import Data.Tropical (Tropical(..))
import Data.Word (Word, Word8, Word16, Word32, Word64)

import qualified Prelude as P
import Prelude (Num(..), Real(..), Double, Float, div, (/))

-- | star(x) = 1 + x * star(x)
--   star x = one `plus` (x `times` (star x))
--
class (Semiring a) => Star a where
  {-# MINIMAL star #-} 
  star :: a -> a

instance Star Bool where
  star _ = True

instance Star Int where
  star i = 1 `div` (1 - i)

instance Star Int8 where
  star i = 1 `div` (1 - i)

instance Star Int16 where
  star i = 1 `div` (1 - i)

instance Star Int32 where
  star i = 1 `div` (1 - i)

instance Star Int64 where
  star i = 1 `div` (1 - i)

instance Star Double where
  star d = 1 / (1 - d)

instance Star Float where
  star f = 1 / (1 - f)

instance Real t => Star (Tropical t) where
  star _ = one

