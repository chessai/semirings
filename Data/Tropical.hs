module Data.Tropical
  ( Tropical(..)
  , isInf
  , addT
  , mulT
  , divT
  , expT
  ) where

data Real t => Tropical t = Tropical { realValue :: t }
                          | Infinity

isInf :: Real t => Tropical t -> Bool
isInf Infinity = True
isInf _        = False

addT :: Real t => Tropical t -> Tropical t -> Tropical t
addT x Infinity = Infinity
addT Infinity x = Infinity
addT x y = Tropical $ min (realValue x) (realValue y)

mulT :: Real t => Tropical t -> Tropical t -> Tropical t
mulT x y
  | isInf x || isInf y = Infinity
  | otherwise = Tropical $ (realValue x) + (realValue y)

divT :: Real t => Tropical t -> Tropical t -> Tropical t
_ `divT` Infinity = undefined
Infinity `divT` _ = Infinity
x `divT` y        = Tropical $ (realValue x) - (realValue y)

expT :: Real t => Tropical t -> Tropical t -> Tropical t
x `expT` y
  | isInf x || isInf y = Infinity
  | otherwise = Tropical $ (realValue x) * (realValue y)
