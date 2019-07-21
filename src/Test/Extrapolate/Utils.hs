-- |
-- Module      : Test.Extrapolate.Utils
-- Copyright   : (c) 2017-2019 Rudy Matela
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
  , listEq,   listOrd
  , maybeEq,  maybeOrd
  , eitherEq, eitherOrd
  , pairEq,   pairOrd
  , tripleEq, tripleOrd
  , quadrupleEq, quadrupleOrd
  , minimumOn
  , maximumOn
  , takeBound
  , nubMergeMap
  , discard
  , compareIndex
  , (.:)
  )
where

import Data.Function (on)
import Data.List (minimumBy, elemIndex)

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

eitherOrd :: (a -> a -> Bool) -> (b -> b -> Bool) -> Either a b -> Either a b -> Bool
eitherOrd (<=) _ (Left  x) (Left  y) = x <= y
eitherOrd _ (<=) (Right x) (Right y) = x <= y
eitherOrd _    _ (Left  _) (Right _) = True
eitherOrd _    _ (Right _) (Left  _) = False

pairEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a,b) -> (a,b) -> Bool
pairEq (==.) (.==) (x1,y1) (x2,y2) = x1 ==. x2 && y1 .== y2

pairOrd :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a,b) -> (a,b) -> Bool
pairOrd (<=.) (.<=) (x1,y1) (x2,y2) = x1 <. x2
                                   || x1 ==. x2 && y1 .<= y2
  where
  x <.  y = x <=. y && not (y <=. x)
  x ==. y = x <=. y &&      y <=. x

tripleEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> (c -> c -> Bool)
         -> (a,b,c) -> (a,b,c) -> Bool
tripleEq (==..) (.==.) (..==) (x1,y1,z1) (x2,y2,z2) =
  x1 ==.. x2 && y1 .==. y2 && z1 ..== z2

tripleOrd :: (a -> a -> Bool) -> (b -> b -> Bool) -> (c -> c -> Bool)
          -> (a,b,c) -> (a,b,c) -> Bool
tripleOrd (<=..) (.<=.) (..<=) (x1,y1,z1) (x2,y2,z2) =
  x1 <.. x2 || x1 ==.. x2 && pairOrd (.<=.) (..<=) (y1,z1) (y2,z2)
  where
  x <..  y = x <=.. y && not (y <=.. x)
  x ==.. y = x <=.. y &&      y <=.. x

quadrupleEq :: (a->a->Bool) -> (b->b->Bool) -> (c->c->Bool) -> (d->d->Bool)
            -> (a,b,c,d) -> (a,b,c,d) -> Bool
quadrupleEq (==...) (.==..) (..==.) (...==) (x1,y1,z1,w1) (x2,y2,z2,w2) =
  x1 ==... x2 && y1 .==.. y2 && z1 ..==. z2 && w1 ...== w2

quadrupleOrd :: (a->a->Bool) -> (b->b->Bool) -> (c->c->Bool) -> (d->d->Bool)
             -> (a,b,c,d) -> (a,b,c,d) -> Bool
quadrupleOrd (<=...) (.<=..) (..<=.) (...<=) (x1,y1,z1,w1) (x2,y2,z2,w2) =
  x1 <... x2 || x1 ==... x2 && tripleOrd (.<=..) (..<=.) (...<=) (y1,z1,w1) (y2,z2,w2)
  where
  x <...  y = x <=... y && not (y <=... x)
  x ==... y = x <=... y &&      y <=... x

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (compare `on` f)

-- left/head-biased, which is different from Prelude's maximum
maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f []     = error "maximumOn: empty list"
maximumOn f [x]    = x
maximumOn f (x:xs) = let y = maximumOn f xs
                     in if f x < f y
                          then y
                          else x

takeBound :: Maybe Int -> [a] -> [a]
takeBound Nothing  xs  =  xs
takeBound (Just n) xs  =  take n xs

nubMerges :: Ord a => [[a]] -> [a]
nubMerges = nubMergesBy compare

nubMergesBy :: Ord a => (a -> a -> Ordering) -> [[a]] -> [a]
nubMergesBy cmp [] = []
nubMergesBy cmp [xs] = xs
nubMergesBy cmp xss = nubMergeBy cmp (nubMerges yss) (nubMerges zss)
  where
  (yss,zss) = splitHalf xss
  splitHalf xs = splitAt (length xs `div` 2) xs

nubMergeMap :: Ord b => (a -> [b]) -> [a] -> [b]
nubMergeMap f = nubMerges . map f

discard :: (a -> Bool) -> [a] -> [a]
discard p = filter (not . p)

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

compareIndex :: Eq a => [a] -> a -> a -> Ordering
compareIndex xs x y =
  case (elemIndex x xs, elemIndex y xs) of
    (Just  i, Just  j) -> i `compare` j
    (Nothing, Just  _) -> GT
    (Just  _, Nothing) -> LT
    _                  -> EQ
