-- |
-- Module      : Test.Extrapolate.TypeBinding
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- Some type binding operators that are useful when defining Generalizable
-- instances.
module Test.Extrapolate.TypeBinding
  ( arg1
  , arg2
  , arg3
  , arg4
  , arg5
  , arg6
  , (==:)
  , argTy1of1
  , argTy1of2, argTy2of2
  , argTy1of3, argTy2of3, argTy3of3
  , argTy1of4, argTy2of4, argTy3of4, argTy4of4
  , argTy1of5, argTy2of5, argTy3of5, argTy4of5, argTy5of5
  , argTy1of6, argTy2of6, argTy3of6, argTy4of6, argTy5of6, argTy6of6
  , module Test.LeanCheck.Utils.TypeBinding
  )
where

import Test.LeanCheck.Utils.TypeBinding

-- TODO: reexport LeanCheck's typebinding operators
-- TODO: or maybe re-export Express's typebinding operators?

arg1 :: (a -> b) -> a
arg1 _  =  undefined

arg2 :: (a -> b -> c) -> b
arg2 _  =  undefined

arg3 :: (a -> b -> c -> d) -> c
arg3 _  =  undefined

arg4 :: (a -> b -> c -> d -> e) -> d
arg4 _  =  undefined

arg5 :: (a -> b -> c -> d -> e -> f) -> e
arg5 _  =  undefined

arg6 :: (a -> b -> c -> d -> e -> f -> g) -> f
arg6 _  =  undefined

(==:) :: (a -> (b -> c -> d)) -> a -> b
x ==: y  =  undefined


argTy1of1 :: con a -> a
argTy1of1 _ = undefined


argTy1of2 :: con a b -> a
argTy1of2 _ = undefined

argTy2of2 :: con a b -> b
argTy2of2 _ = undefined


argTy1of3 :: con a b c -> a
argTy1of3 _ = undefined

argTy2of3 :: con a b c -> b
argTy2of3 _ = undefined

argTy3of3 :: con a b c -> c
argTy3of3 _ = undefined


argTy1of4 :: con a b c d -> a
argTy1of4 _ = undefined

argTy2of4 :: con a b c d -> b
argTy2of4 _ = undefined

argTy3of4 :: con a b c d -> c
argTy3of4 _ = undefined

argTy4of4 :: con a b c d -> d
argTy4of4 _ = undefined


argTy1of5 :: con a b c d e -> a
argTy1of5 _ = undefined

argTy2of5 :: con a b c d e -> b
argTy2of5 _ = undefined

argTy3of5 :: con a b c d e -> c
argTy3of5 _ = undefined

argTy4of5 :: con a b c d e -> d
argTy4of5 _ = undefined

argTy5of5 :: con a b c d e -> e
argTy5of5 _ = undefined


argTy1of6 :: con a b c d e f -> a
argTy1of6 _ = undefined

argTy2of6 :: con a b c d e f -> b
argTy2of6 _ = undefined

argTy3of6 :: con a b c d e f -> c
argTy3of6 _ = undefined

argTy4of6 :: con a b c d e f -> d
argTy4of6 _ = undefined

argTy5of6 :: con a b c d e f -> e
argTy5of6 _ = undefined

argTy6of6 :: con a b c d e f -> f
argTy6of6 _ = undefined
