{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}

-- below are safe orphan instances
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Semiring.Generic
  ( GSemiring(..)
  , GRing(..)
  ) where 

import           Data.Semiring
import           GHC.Generics

import Prelude hiding (Num(..))

instance (Semiring a, Semiring b) => Semiring (a,b) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes; 

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a,b,c,d,e) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f) => Semiring (a,b,c,d,e,f) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f, Semiring g) => Semiring (a,b,c,d,e,f,g) where
  zero = defZero; one = defOne; plus = defPlus; times = defTimes;

instance (Ring a, Ring b) => Ring (a,b) where
  negate = defNegate

instance (Ring a, Ring b, Ring c) => Ring (a,b,c) where
  negate = defNegate

instance (Ring a, Ring b, Ring c, Ring d) => Ring (a,b,c,d) where
  negate = defNegate

instance (Ring a, Ring b, Ring c, Ring d, Ring e) => Ring (a,b,c,d,e) where
  negate = defNegate

instance (Ring a, Ring b, Ring c, Ring d, Ring e, Ring f) => Ring (a,b,c,d,e,f) where
  negate = defNegate

instance (Ring a, Ring b, Ring c, Ring d, Ring e, Ring f, Ring g) => Ring (a,b,c,d,e,f,g) where
  negate = defNegate

{--------------------------------------------------------------------
  Generics
--------------------------------------------------------------------}

class GSemiring f where
  {-# MINIMAL gPlus, gZero, gTimes, gOne #-} 
  gZero  :: f a
  gOne   :: f a
  gPlus  :: f a -> f a -> f a
  gTimes :: f a -> f a -> f a

class GRing f where
  {-# MINIMAL gNegate #-}
  gNegate :: f a -> f a

defZero, defOne   :: (Generic a, GSemiring (Rep a)) => a
defPlus, defTimes :: (Generic a, GSemiring (Rep a)) => a -> a -> a
defZero = to gZero
defOne  = to gOne
defPlus x y = to $ from x `gPlus` from y
defTimes x y = to $ from x `gTimes` from y

defNegate :: (Generic a, GRing (Rep a)) => a -> a
defNegate x = to $ gNegate $ from x

instance GSemiring U1 where
  gZero = U1
  gOne  = U1
  gPlus  _ _ = U1
  gTimes _ _ = U1

instance GRing U1 where
  gNegate _ = U1

instance (GSemiring a, GSemiring b) => GSemiring (a :*: b) where
  gZero = gZero :*: gZero
  gOne  = gOne  :*: gOne
  gPlus  (a :*: b) (c :*: d) = gPlus  a c :*: gPlus b d
  gTimes (a :*: b) (c :*: d) = gTimes a c :*: gPlus b d

instance (GRing a, GRing b) => GRing (a :*: b) where
  gNegate (a :*: b) = gNegate a :*: gNegate b

instance (GSemiring a) => GSemiring (M1 i c a) where
  gZero = M1 gZero
  gOne  = M1 gOne
  gPlus  (M1 x) (M1 y) = M1 $ gPlus  x y
  gTimes (M1 x) (M1 y) = M1 $ gTimes x y

instance (GRing a) => GRing (M1 i c a) where
  gNegate (M1 x) = M1 $ gNegate x

instance (Semiring a) => GSemiring (K1 i a) where
  gZero = K1 zero
  gOne  = K1 one
  gPlus  (K1 x) (K1 y) = K1 $ plus  x y
  gTimes (K1 x) (K1 y) = K1 $ times x y

instance (Ring a) => GRing (K1 i a) where
  gNegate (K1 x) = K1 $ negate x
