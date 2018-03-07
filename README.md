Extrapolate
===========

[![Extrapolate Build Status][build-status]][build-log]
[![Extrapolate on Hackage][hackage-version]][extrapolate-on-hackage]
[![Extrapolate on Stackage LTS][stackage-lts-badge]][extrapolate-on-stackage-lts]
[![Extrapolate on Stackage Nightly][stackage-nightly-badge]][extrapolate-on-stackage-nightly]

Extrapolate is a [property-based testing] library for [Haskell]
capable of reporting generalized counter-examples.


Installing Extrapolate
----------------------

To install the latest version of [Extrapolate from Hackage] using [cabal], just:

	$ cabal update
	$ cabal install extrapolate

To test if it installed correctly, follow through the next section.


Using Extrapolate
-----------------

To use Extrapolate, you first import the [`Test.Extrapolate`] module,
then pass any properties you with to test to the function [`check`]:

	$ ghci
	> import Test.Extrapolate
	> check $ \x y -> x + y == y + (x :: Int)
	+++ OK, passed 360 tests.

	> import Data.List (nub)
	> check $ \xs -> nub xs == (xs :: [Int])
	*** Failed! Falsifiable (after 3 tests):
	[0,0]

	Generalization:
	x:x:_

The operator [`+`] is commutative.  The function [`nub`] is not an identity.


### Configuring the number of tests

To increase the number of tests, use the [`for`] combinator:

	$ ghci
	> import Test.Extrapolate
	> check `for` 1000 $ \x y -> x + y == y + (x :: Int)
	+++ OK, passed 1000 tests.


### Customizing the background functions (allowed in side-conditions)

To customize the background functions, use the [`withBackground`] combinator:

	$ ghci
	> import Test.Extrapolate
	> import Data.List (nub)
	> let hasDups xs  =  nub xs /= (xs :: [Int])
	> check `withBackground` [constant "hasDups" hasDups] $ \xs -> nub xs == (xs :: [Int])
	*** Failed! Falsifiable (after 3 tests):
	[0,0]

	Generalization:
	x:x:_

	Conditional Generalization:
	xs  when  hasDups xs

Perhaps the example above is silly (`hasDups` is the negation of the property
itself!), but it illustrates the use of [`withBackground`].


The combinators [`for`] and [`withBackground`] can be used in conjunction:

	> check `for` 100 `withBackground` [...] $ property

Don't forget the dollar sign [`$`].


Another Example
---------------

Consider the following (faulty) sort function and a property about it:

    sort :: Ord a => [a] -> [a]
    sort []      =  []
    sort (x:xs)  =  sort (filter (< x) xs)
                 ++ [x]
                 ++ sort (filter (> x) xs)

    prop_sortCount :: Ord a => a -> [a] -> Bool
    prop_sortCount x xs  =  count x (sort xs) == count x xs
      where
      count x = length . filter (== x)

After testing the property, Extrapolate returns a fully defined counter-example
along with a generalization:

    > import Test.Extrapolate
    > check (prop_sortCount :: Int -> [Int] -> Bool)
    *** Failed! Falsifiable (after 4 tests):
    0 [0,0]

    Generalization:
    x (x:x:_)

This hopefully makes it easier to find the source of the bug.  In this case,
the faulty sort function discards repeated elements.


Further reading
---------------

For more examples, see the [eg](eg) folder.
For type signatures, other options and uses,
see [Extrapolate's API documentation].

There are two other tools for [Haskell] capable of producing generalized
counter-examples: [SmartCheck] and [Lazy SmallCheck 2012].

Extrapolate was accepted for presentation at [IFL 2017],
see the pre-symposium proceedings
[paper about Extrapolate](https://matela.com.br/paper/extrapolate.pdf).
Extrapolate is also subject to a chapter in a [PhD Thesis (2017)].

[extrapolate-on-hackage]:          https://hackage.haskell.org/package/extrapolate
[Extrapolate from Hackage]:        https://hackage.haskell.org/package/extrapolate
[Extrapolate's API documentation]: https://hackage.haskell.org/package/extrapolate/docs/Test-Extrapolate.html
[`Test.Extrapolate`]:              https://hackage.haskell.org/package/extrapolate/docs/Test-Extrapolate.html
[`check`]:                         https://hackage.haskell.org/package/extrapolate/docs/Test-Extrapolate.html#v:check
[`for`]:                           https://hackage.haskell.org/package/extrapolate/docs/Test-Extrapolate.html#v:for
[`withBackground`]:                https://hackage.haskell.org/package/extrapolate/docs/Test-Extrapolate.html#v:withBackground
[`$`]:                             https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:-36-
[`+`]:                             https://hackage.haskell.org/package/base/docs/Prelude.html#v:-43-
[`nub`]:                           https://hackage.haskell.org/package/base/docs/Data-List.html#v:nub
[Haskell]:                         https://www.haskell.org/
[cabal]:                           https://www.haskell.org/cabal/
[property-based testing]:          https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md

[IFL 2017]:             http://iflconference.org/
[SmartCheck]:           https://github.com/leepike/SmartCheck
[Lazy SmallCheck 2012]: https://github.com/UoYCS-plasma/lazysmallcheck2012
[PhD Thesis (2017)]: https://matela.com.br/paper/rudy-phd-thesis-2017.pdf

[build-status]:    https://travis-ci.org/rudymatela/extrapolate.svg?branch=master
[build-log]:       https://travis-ci.org/rudymatela/extrapolate
[hackage-version]: https://img.shields.io/hackage/v/extrapolate.svg
[stackage-lts-badge]:            http://stackage.org/package/extrapolate/badge/lts
[stackage-nightly-badge]:        http://stackage.org/package/extrapolate/badge/nightly
[extrapolate-on-stackage]:         http://stackage.org/package/extrapolate
[extrapolate-on-stackage-lts]:     http://stackage.org/lts/package/extrapolate
[extrapolate-on-stackage-nightly]: http://stackage.org/nightly/package/extrapolate
