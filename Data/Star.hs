module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Semiring (Semiring(..))
import Data.Word (Word, Word8, Word16, Word32, Word64)

import qualified Prelude as P
import Prelude (Num(..), Real(..), Double, Float, div, (/))

class (Semiring a) => Star a where
  {-# MINIMAL star | asterplus #-} 
  star :: a -> a
  star a = one `plus` asterplus a

  asterplus :: a -> a
  asterplus a = a `times` star a

instance Star Bool where
  star _ = True
