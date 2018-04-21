{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -Wall #-}

-- below are safe orphan instances
{-# OPTIONS_GHC -fno-warn-orphans #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semiring.Generic
-- Copyright   :  (C) 2018 chessai
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  chessai <chessai1996@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides generic deriving tools for semirings and rings for
-- product-like structures.
--
----------------------------------------------------------------------------

module Data.Semiring.Generic
  ( 
#if MIN_VERSION_base(4,6,0) 
    GSemiring(..)
  , gzero
  , gone
  , gplus
  , gtimes
  , GRing(..)
  , gnegate
#endif
  ) where 

#if MIN_VERSION_base(4,6,0)
import           Data.Semiring
import           GHC.Generics

import Prelude hiding (Num(..))

instance (Semiring a, Semiring b) => Semiring (a,b) where
  zero = gzero; one = gone; plus = gplus; times = gtimes; 

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
  zero = gzero; one = gone; plus = gplus; times = gtimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
  zero = gzero; one = gone; plus = gplus; times = gtimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a,b,c,d,e) where
  zero = gzero; one = gone; plus = gplus; times = gtimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f) => Semiring (a,b,c,d,e,f) where
  zero = gzero; one = gone; plus = gplus; times = gtimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g) => Semiring (a,b,c,d,e,f,g) where
  zero = gzero; one = gone; plus = gplus; times = gtimes;

instance (Ring a, Ring b) => Ring (a,b) where
  negate = gnegate

instance (Ring a, Ring b, Ring c) => Ring (a,b,c) where
  negate = gnegate

instance (Ring a, Ring b, Ring c, Ring d) => Ring (a,b,c,d) where
  negate = gnegate

instance (Ring a, Ring b, Ring c, Ring d, Ring e) => Ring (a,b,c,d,e) where
  negate = gnegate

instance (Ring a, Ring b, Ring c, Ring d, Ring e, Ring f) => Ring (a,b,c,d,e,f) where
  negate = gnegate

instance (Ring a, Ring b, Ring c, Ring d, Ring e, Ring f, Ring g) => Ring (a,b,c,d,e,f,g) where
  negate = gnegate

{--------------------------------------------------------------------
  Generics
--------------------------------------------------------------------}

class GSemiring f where
  {-# MINIMAL gplus', gzero', gtimes', gone' #-} 
  gzero'  :: f a
  gone'   :: f a
  gplus'  :: f a -> f a -> f a
  gtimes' :: f a -> f a -> f a

class GRing f where
  {-# MINIMAL gnegate' #-}
  gnegate' :: f a -> f a

-- | Generically generate a 'Semiring' 'zero' for any product-like     type
-- implementing 'Generic'.
--
-- It is only defined for product types.
--
-- @
-- 'gplus' 'gzero' a = a = 'gplus' a 'gzero'
-- @
gzero :: (Generic a, GSemiring (Rep a)) => a
gzero = to gzero'

-- | Generically generate a 'Semiring' 'one' for any product-like type
-- implementing 'Generic'.
--
-- It is only defined for product types.
--
-- @
-- 'gtimes' 'gone' a = a = 'gtimes' a 'gone'
-- @
gone :: (Generic a, GSemiring (Rep a)) => a
gone  = to gone'

-- | Generically generate a 'Semiring' 'plus' operation for any type
-- implementing 'Generic'. It is only defined for product types.
--
-- @
-- 'gplus' a b = 'gplus' b a
-- @
gplus :: (Generic a, GSemiring (Rep a)) => a -> a -> a
gplus x y = to $ from x `gplus'` from y

-- | Generically generate a 'Semiring' 'times' operation for any type
-- implementing 'Generic'. It is only defined for product types.
--
-- @
-- 'gtimes' a ('gtimes' b c) = 'gtimes' ('gtimes' a b) c
-- 'gtimes' a 'gzero' = 'gzero' = 'gtimes' 'gzero' a
-- @
gtimes :: (Generic a, GSemiring (Rep a)) => a -> a -> a
gtimes x y = to $ from x `gtimes'` from y

-- | Generically generate a 'Ring' 'negate' operation for any type
-- implementing 'Generic'. It is only defined for product types.
--
-- @
-- 'gplus' a ('gnegate' a) = 'zero'
-- @
gnegate :: (Generic a, GRing (Rep a)) => a -> a
gnegate x = to $ gnegate' $ from x

instance GSemiring U1 where
  gzero' = U1
  gone'  = U1
  gplus'  _ _ = U1
  gtimes' _ _ = U1

instance GRing U1 where
  gnegate' _ = U1

instance (GSemiring a, GSemiring b) => GSemiring (a :*: b) where
  gzero' = gzero' :*: gzero'
  gone'  = gone'  :*: gone'
  gplus'  (a :*: b) (c :*: d) = gplus'  a c :*: gplus' b d
  gtimes' (a :*: b) (c :*: d) = gtimes' a c :*: gplus' b d

instance (GRing a, GRing b) => GRing (a :*: b) where
  gnegate' (a :*: b) = gnegate' a :*: gnegate' b

instance (GSemiring a) => GSemiring (M1 i c a) where
  gzero' = M1 gzero'
  gone'  = M1 gone'
  gplus'  (M1 x) (M1 y) = M1 $ gplus'  x y
  gtimes' (M1 x) (M1 y) = M1 $ gtimes' x y

instance (GRing a) => GRing (M1 i c a) where
  gnegate' (M1 x) = M1 $ gnegate' x

instance (Semiring a) => GSemiring (K1 i a) where
  gzero' = K1 zero
  gone'  = K1 one
  gplus'  (K1 x) (K1 y) = K1 $ plus  x y
  gtimes' (K1 x) (K1 y) = K1 $ times x y

instance (Ring a) => GRing (K1 i a) where
  gnegate' (K1 x) = K1 $ negate x
#endif
