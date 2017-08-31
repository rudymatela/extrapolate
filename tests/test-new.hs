-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.List (sort, nub, union)

import Test.Extrapolate.New

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  -- tests about lgg
  , lggOK n int
  , lggOK n [int]
  , lggOK n [[int]]
  , lggOK n [([int],eith int char)]
  , lgg1 (expr [0,0::Int]) (expr [1,1::Int])    ==  _i -:- _i -:- ll
  , lgg1 (expr [0,1::Int]) (expr [1,0::Int])    ==  _i -:- _i -:- ll
  , lgg1 (expr [0,0::Int]) (expr [0,0,1::Int])  ==  zero -:- zero -:- _is
  , lgg1 (expr [1,1::Int]) (expr [2,2,2::Int])  ==  _i -:- _i -:- _is
  , lgg1 (expr (Just (0::Int))) (expr (Nothing :: Maybe Int)) == _mi
  , lgg1 (expr (Just (0::Int))) (expr (Just (1 :: Int)))      == just _i
  , lgg1 (expr [Just 1, Just (0::Int)]) (expr [Nothing, Just (1::Int)])
    ==  _mi -:- just _i -:- llmi

  , head (generalizedCounterExamples 360 $ \xs -> nub xs == (xs::[Int]))
    == [_i -:- _i -:- _is]
  , head (generalizedCounterExamples 360 $ \xs ys -> xs `union` ys == ys `union` (xs::[Int]))
    == [ll, zero -:- zero -:- ll]
-- but should be:
--  == [_is, _i -:- _i -:- _is]
  ]

lggOK :: Generalizable a => Int -> a -> Bool
lggOK n x = holds n (lggCommutative -:> x)
         && holds n (lggAssociative -:> x)
         && holds n (lggIdempotent -:> x)

lggCommutative :: Generalizable a => a -> a -> Bool
lggCommutative x y = lgg1 (expr x) (expr y) == lgg1 (expr y) (expr x)

lggAssociative :: Generalizable a => a -> a -> a -> Bool
lggAssociative x y z = (associative lgg1) (expr x) (expr y) (expr z)

lggIdempotent :: Generalizable a => a -> a -> Bool
lggIdempotent x y = let z = expr x `lgg1` expr y
                    in  z == (z `lgg1` expr y)
