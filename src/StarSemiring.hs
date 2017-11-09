module StarSemiring where

import Data.Maybe (listToMaybe)

infixl 6 <+>
infixl 7 <.>

{- Laws:
 - a <+> b = b <+> a
 - (a <+> b) <+> c = a <+> (b <+> c)
 - a <+> zero = zero <+> a = a
 - (a <.> b) <.> c = a <.> (b <.> c)
 - a <.> one = one <.> a = a
 - a <.> zero = zero <.> a = zero
 - a <.> (b <+> c) = a <.> b <+> a <.> c
 - (a <+> b) <.> c = a <.> c <+> b <.> c
-}
class Semiring a where
  zero  :: a
  (<+>) :: a -> a -> a
  one   :: a
  (<.>) :: a -> a -> a
  srsum :: [a] -> a
  srsum = foldr (<+>) zero
  srprod :: [a] -> a
  srprod = foldr (<.>) one

{- Laws:
 - star a = one <+> a <.> star a = one <+> star a <.> a
-}
class Semiring a => StarSemiring a where
  star :: a -> a
  star a = one <+> plus a
  plus :: a -> a
  plus a = a <.> star a

{- Laws:
 - a <+> a = a
 - a <.> x <+> x = x ==> star a <.> x <+> x = x
 - x <.> a <+> x = x ==> x <.> star a <+> x = x
-}

class StarSemiring a => KleeneAlgebra a where

data StarSemiringExpr a = Var a
                        | Or (StarSemiringExpr a) (StarSemiringExpr a)
                        | Seq (StarSemiringExpr a) (StarSemiringExpr a)
                        | Star (StarSemiringExpr a)
                        | None
                        | Empty

newtype RE a = RE (StarSemiringExpr a)

re :: a -> RE a
re = RE . Var

instance Eq a => Eq (RE a) where
  (RE x) == (RE y) = todo

-- While we could simply implement *-semiring operations directly as the constructors of StarSemiringExpr, instead we will take advantage of this opportunity to implement a few local simplifications: identities of ε and 0, absorption of 0, idempotency of asteration, and the following theorems of regular expressions:
-- ε + ε = ε
-- ε + x∗ = x∗
-- x∗ + ε = x∗

instance Semiring (RE a) where
  zero = RE None
  RE None <+> y = y
  x <+> RE None = x
  RE Empty <+> RE Empty = RE Empty
  RE Empty <+> RE (Star y) = RE (Star y)
  RE (Star x) <+> RE Empty = RE (Star x)
  RE x <+> RE y = RE (x `Or` y)
  one = RE Empty
  RE Empty <.> y = y
  x <.> RE Empty = x
  RE None <.> _  = RE None
  _ <.> RE None  = RE None
  RE x <.> RE y  = RE (x `Seq` y)

instance StarSemiring (RE a) where
  star (RE None) = RE Empty
  star (RE Empty) = RE Empty
  star (RE (Star x)) = star (RE x)
  star (RE x) = RE (Star x)

data Tropical a = Tropical a
                | Infinity deriving (Eq, Ord)

instance (Ord a, Num a) => Semiring (Tropical a) where
  zero = Infinity
  Infinity <+> y = y
  x <+> Infinity = x
  (Tropical a) <+> (Tropical b) = Tropical (min a b)
  one = Tropical 0
  Infinity <.> _ = Infinity
  _ <.> Infinity = Infinity
  (Tropical x) <.> (Tropical y) = Tropical (x + y)

instance (Ord a, Num a) => StarSemiring (Tropical a) where
  star _ = one

instance (Ord a, Num a) => KleeneAlgebra (Tropical a) where 

data ShortestPath a b = ShortestPath (Tropical a) b

instance Functor (ShortestPath a) where
  fmap f (ShortestPath a x) = ShortestPath a (f x)

extract :: ShortestPath a b -> b
extract (ShortestPath _ x) = x

instance (Ord a, Num a, Semiring b) => Semiring (ShortestPath a b) where
  zero = ShortestPath zero zero
  ShortestPath a x <+> ShortestPath b y | c < b = ShortestPath a x
                                        | c < a = ShortestPath b y
                                        | otherwise = ShortestPath c (x <+> y)
    where
      c = a <+> b
  one = ShortestPath one one
  ShortestPath a x <.> ShortestPath b y = ShortestPath (a <.> b) (x <.> y)

instance (Ord a, Num a, StarSemiring b) => StarSemiring (ShortestPath a b) where
  star (ShortestPath x b) | x == one  = ShortestPath one (star b)
                          | otherwise = ShortestPath one one

instance (Ord a, Num a, KleeneAlgebra b) => KleeneAlgebra (ShortestPath a b) where

newtype Language a = Language [[a]]

letter x = Language [[x]]

instance Semiring (Language a) where
  zero = Language []
  (Language x) <+> (Language y) = Language (x `interleave` y)
   where
    []     `interleave` ys = ys
    (x:xs) `interleave` ys = x:(ys `interleave` xs)
  one = Language (pure [])
  (Language x) <.> (Language y) = Language (dovetail (++) x y)
   where
    dovetail f l1 l2 = concat $ go l1 (scanl (flip (:)) [] l2)
     where
      go [] _           = []
      go l1 l2@(x:y:ys) = (zipWith f l1 x):(go l1 (y:ys))
      go l1@(a:as) [x]  = (zipWith f l1 x):(go as [x])

instance StarSemiring (Language a) where
  star (Language l) = one <+> plusList (filter (not . null) l)
    where
      plusList [] = zero
      plusList l  = star (Language l) <.> (Language l)

someWord :: Language a -> Maybe [a]
someWord (Language l) = listToMaybe l

evalRE :: (KleeneAlgebra a) => (l -> a) -> RE l -> a
evalRE f (RE None) = zero
evalRE f (RE Empty) = one
evalRE f (RE (Var a)) = f a
evalRE f (RE (Star x)) = star (evalRE f (RE x))
evalRE f (RE (x `Or` y)) = (evalRE f (RE x)) <+> (evalRE f (RE y))
evalRE f (RE (x `Seq` y)) = (evalRE f (RE x)) <.> (evalRE f (RE y))

data Compact a = Real a
               | Inf

instance (Eq a, Num a) => Semiring (Compact a) where
  zero = Real 0
  Inf <+> _ = Inf
  _ <+> Inf = Inf
  Real x <+> Real y = Real (x + y)
  one = Real 1
  Real 0 <.> _ = Real 0
  _ <.> Real 0 = Real 0
  Inf <.> _ = Inf
  _ <.> Inf = Inf
  Real x <.> Real y = Real (x * y)

instance (Eq a, Fractional a) => StarSemiring (Compact a) where
  star (Real 1) = Inf
  star (Real x) = Real (recip (1 - x))
  star Inf      = Inf

instance Semiring (StarSemiringExpr a) where
  zero = None
  None <+> y = y
  x <+> None = x
  x <+> y = x `Or` y
  one = Empty
  Empty <.> y = y
  x <.> Empty = x
  None <.> _ = None
  _ <.> None = None
  x <.> y = x `Seq` y

instance StarSemiring (StarSemiringExpr a) where
  star None = Empty
  star x    = Star x

evalSSE :: (StarSemiring a) => (l -> a) -> StarSemiringExpr l -> a
evalSSE f None        = zero
evalSSE f Empty       = one
evalSSE f (Var a)     = f a
evalSSE f (Star x)    = star (evalSSE f x)
evalSSE f (x `Or` y)  = (evalSSE f x) <+> (evalSSE f y)
evalSSE f (x `Seq` y) = (evalSSE f x) <.> (evalSSE f y)
-- | Below you will find an appendix of functions that completes this module.
--

instance Show a => Show (StarSemiringExpr a) where
  showsPrec d (Var a) = showParen (d > 10) (shows a)
  showsPrec d Empty = showParen (d > 10) (showString "ε")
  showsPrec d None = showParen (d > 10) (showString "0")
  showsPrec d (Star x) = showParen (d > 9) (showsPrec 9 x . showString "*")
  showsPrec d (x `Or` y) = showParen (d > 6) showStr
   where
    showStr = showsPrec 6 x . showString "|" . showsPrec 6 y
  showsPrec d (x `Seq` y) = showParen (d > 7) showStr
   where
    showStr = showsPrec 7 x . showsPrec 7 y

instance Show a => Show (RE a) where
  showsPrec d (RE x) = showsPrec d x

instance Show a => Show (Tropical a) where
  show (Tropical a) = show a
  show Infinity = "∞"

instance (Show a, Show b) => Show (ShortestPath a b) where
  show (ShortestPath a x) = show x ++ "[" ++ show a ++ "]"

instance (Show a) => Show (Compact a) where
  show (Real a) = show a
  show Inf = "∞"

todo = error "TODO"
