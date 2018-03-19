semirings
==========

Haskellers are usually familiar with monoids and semigroups. A monoid has an appending operation `<>` or `mappend` and an identity element `mempty`. A semigroup has an append `<>`, but does not require an `mempty` element.

A Semiring has two appending operations, 'plus' and 'times', and two respective identity elements, 'zero' and 'one'.

More formally, A semiring <i>R</i> is a set equipped with two binary relations + and *, such that:

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

A derived operation called "asterplus" can be defined in terms of star by:

- star a = 1 + asterplus a
- asterplus :: a -> a
- asterplus a = a * star a

As such, a minimal instance of the typeclass 'Star' requires only 'star' or 'asterplus' to be defined.

use cases
=========

semirings themselves are useful as a way to express that a type is both an additive and multiplicative monoid.

*-semirings are useful in a number of applications; such as matrix algebra, regular expressions, kleene algebras, graph theory, tropical algebra, dataflow analysis, power series, linear recurrence relations.

Some relevant reading material:

http://stedolan.net/research/semirings.pdf <br>
http://r6.ca/blog/20110808T035622Z.html <br>
https://byorgey.wordpress.com/2016/04/05/the-network-reliability-problem-and-star-semirings/ <br>
