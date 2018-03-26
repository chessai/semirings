module Data.Star
  ( Star(..)
  ) where

import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Prelude as P
import Prelude (id)

import Data.Semiring

class (Semiring a) => Star a where
  {-# MINIMAL star | aplus #-} 
  star :: a -> a
  star a = one `plus` aplus a

  aplus :: a -> a
  aplus a = a `times` star a

instance Star Bool where
  star _  = True
  aplus   = id

instance Star () where
  star  _ = ()
  aplus _ = ()

instance Star a => Star [a] where
  star [] = one
  star (x:xs) = r where
    r = xst : P.fmap (xst *) (xs * r)
    xst = star x


