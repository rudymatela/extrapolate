-- |
-- Module      : Test.Extrapolate.Utils
-- Copyright   : (c) 2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- Misc. utilities.
module Test.Extrapolate.Utils
  ( (+++)
  , nubMerge
  , nubMergeOn
  , nubMergeBy
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
