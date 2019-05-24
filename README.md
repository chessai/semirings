semirings
==========
[![Hackage](https://img.shields.io/hackage/v/semirings.svg)](https://hackage.haskell.org/package/semirings) [![Build Status](https://travis-ci.org/chessai/semirings.svg?branch=master)](https://travis-ci.org/chessai/semirings)

Haskellers are usually familiar with monoids and semigroups. A monoid has an appending operation `<>` or `mappend` and an identity element `mempty`. A semigroup has an append `<>`, but does not require an `mempty` element.

A Semiring has two appending operations, 'plus' and 'times', and two respective identity elements, 'zero' and 'one'.

More formally, A semiring R is a set equipped with two binary relations + and *, such that:

- (R, +) is a commutative monoid with identity element 0:
  - (a + b) + c = a + (b + c)
  - 0 + a = a + 0 = a
  - a + b = b + a
- (R, *) is a monoid with identity element 1:
  - (a * b) * c = a * (b * c)
  - 1 * a = a * 1 = a
- Multiplication left and right distributes over addition
  - a * (b + c) = (a * b) + (a * c)
  - (a + b) * c = (a * c) + (b * c)
- Multiplication by '0' annihilates R:
  - 0 * a = a * 0 = 0

*-semirings
===========

A *-semiring (pron. "star-semiring") is any semiring with an additional operation 'star' (read as "asteration"), such that:

- star a = 1 + a * star a = 1 + star a * a

A derived operation called "aplus" can be defined in terms of star by:

- star :: a -> a
- star a = 1 + aplus a
- aplus :: a -> a
- aplus a = a * star a

As such, a minimal instance of the typeclass 'Star' requires only 'star' or 'aplus' to be defined.

use cases
=========

semirings themselves are useful as a way to express that a type that supports a commutative and associative operation.
Some examples:

- Numbers {Int, Integer, Word, Double, etc.}:
  - 'plus' is 'Prelude.+'
  - 'times' is 'Prelude.*'
  - 'zero' is 0.
  - 'one' is 1.
- Booleans:
  - 'plus' is '||'
  - 'times' is '&&'
  - 'zero' is 'False'
  - 'one' is 'True'
- Set:
  - 'plus' is 'union'
  - 'times' is 'intersection'
  - 'zero' is the empty Set.
  - 'one' is the singleton Set containing the 'one' element of the underlying type.
- NFA:
  - 'plus' unions two NFAs.
  - 'times' appends two NFAs.
  - 'zero' is the NFA that acceptings nothing.
  - 'one' is the empty NFA.
- DFA:
  - 'plus' unions two DFAs.
  - 'times' intersects two DFAs.
  - 'zero' is the DFA that accepts nothing.
  - 'one' is the DFA that accepts everything.

*-semirings are useful in a number of applications; such as matrix algebra, regular expressions, kleene algebras, graph theory, tropical algebra, dataflow analysis, power series, and linear recurrence relations.

Some relevant (informal) reading material:

http://stedolan.net/research/semirings.pdf

http://r6.ca/blog/20110808T035622Z.html

https://byorgey.wordpress.com/2016/04/05/the-network-reliability-problem-and-star-semirings/

additional credit
======

Some of the code in this library was lifted directly from the Haskell library 'semiring-num'.
