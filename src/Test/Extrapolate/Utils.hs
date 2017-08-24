-- |
-- Module      : Test.Extrapolate.Utils
-- Copyright   : (c) 2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- Miscellaneous utility functions.
--
-- This is not intended to be used by users of Extrapolate, only by modules of
-- Extrapolate itself.  Expect symbols exported here to come and go with every
-- minor version.
module Test.Extrapolate.Utils
  ( (+++)
  , nubMerge
  , nubMergeOn
  , nubMergeBy
  , foldr0
  , fromLeft
  , fromRight
  , elemBy
  , listEq   , listOrd
  , maybeEq  , maybeOrd
  , eitherEq
  , (.:)
  )
where

import Data.Function (on)

nubMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
nubMergeBy cmp (x:xs) (y:ys) = case x `cmp` y of
                                 LT -> x:nubMergeBy cmp xs (y:ys)
                                 GT -> y:nubMergeBy cmp (x:xs) ys
                                 EQ -> x:nubMergeBy cmp xs ys
nubMergeBy _ xs ys = xs ++ ys

nubMergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
nubMergeOn f = nubMergeBy (compare `on` f)

nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge = nubMergeBy compare

(+++) :: Ord a => [a] -> [a] -> [a]
(+++) = nubMerge
infixr 5 +++

-- variation of foldr that only uses "zero" when the list is empty
foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 f z xs | null xs   = z
              | otherwise = foldr1 f xs

-- note these versions of fromLeft and fromRight differ from the ones of
-- Data.Either since 4.10.0.0.
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft: not a left"

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight: not a right"

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy (==) x = any (== x)

listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq (==) []     []     = True
listEq (==) (x:xs) []     = False
listEq (==) []     (y:ys) = False
listEq (==) (x:xs) (y:ys) = x == y && listEq (==) xs ys

listOrd :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listOrd (<=) []     []     = True
listOrd (<=) (x:xs) []     = False
listOrd (<=) []     (y:ys) = True
listOrd (<=) (x:xs) (y:ys) = x <  y
                          || x == y && listOrd (<=) xs ys
  where
  x <  y = x <= y && not (y <= x)
  x == y = x <= y &&      y <= x

maybeEq :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeEq (==) Nothing  Nothing  = True
maybeEq (==) Nothing  (Just y) = False
maybeEq (==) (Just x) Nothing  = False
maybeEq (==) (Just x) (Just y) = x == y

maybeOrd :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeOrd (<=) Nothing  Nothing  = True
maybeOrd (<=) Nothing  (Just y) = True
maybeOrd (<=) (Just x) Nothing  = False
maybeOrd (<=) (Just x) (Just y) = x <= y

eitherEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> Either a b -> Either a b -> Bool
eitherEq (==) _ (Left  x) (Left  y) = x == y
eitherEq _ (==) (Right x) (Right y) = x == y
eitherEq _ _ _ _ = False

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
