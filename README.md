Extrapolate
===========

[![Extrapolate Build Status][build-status]][build-log]
[![Extrapolate on Hackage][hackage-version]][extrapolate-on-hackage]

Extrapolate is a property-based testing library for Haskell
capable of reporting generalized counter-examples.


Example
-------

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
    x (x:x:xs)

This hopefully makes it easier to find the source of the bug.  In this case,
the faulty sort function discards repeated elements.


More documentation
------------------

For more examples, see the [eg](eg) folder.

[build-status]: https://travis-ci.org/rudymatela/extrapolate.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/extrapolate
[hackage-version]: https://img.shields.io/hackage/v/extrapolate.svg
[extrapolate-on-hackage]: https://hackage.haskell.org/package/extrapolate
