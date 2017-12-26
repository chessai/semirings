module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Semiring (Semiring(..))
import Data.Word (Word, Word8, Word16, Word32, Word64)

import qualified Prelude as P
import Prelude (Num(..), Double, Float, div, (/))

class (Semiring a) => Star a where
  star :: a -> a
  
--star x = one `plus` (x `times` (star x))

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


