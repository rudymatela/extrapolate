-- |
-- Module      : Test.Extrapolate.Generalization
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This defines utilities for unconditional generalization.
--
-- You are probably better off importing "Test.Extrapolate".
module Test.Extrapolate.Generalization
  ( counterExampleGeneralizations

  , candidateGeneralizations
  , fastCandidateGeneralizations
  , candidateHoleGeneralizations

  , module Test.Extrapolate.Expr
  )
where

import Test.LeanCheck.Error (errorToFalse)

import Test.Extrapolate.Expr

-- |
-- Given boolean expression representing a counter-example,
-- returns all possible unconditional generalizations
-- from most general to least general.
-- All according to a given function that lists ground expressions.
--
-- > > counterExampleGeneralizations (groundsFor not) false
-- > []
--
-- > > counterExampleGeneralizations (groundsFor not) (false -&&- true)
-- > [False && _ :: Bool]
--
-- > > counterExampleGeneralizations (groundsFor not) (false -||- true)
-- > []
--
-- > > counterExampleGeneralizations (groundsFor not) (false -/=- false)
-- > [p /= p :: Bool]
--
-- > > counterExampleGeneralizations (groundsFor not) (false -&&- true -&&- false)
-- > [ (False && _) && _ :: Bool
-- > , (False && p) && p :: Bool
-- > , _ && False :: Bool
-- > , (_ && _) && False :: Bool
-- > , (p && p) && False :: Bool
-- > , (_ && True) && False :: Bool
-- > , (False && _) && False :: Bool
-- > , (False && True) && _ :: Bool
-- > ]
counterExampleGeneralizations :: (Expr -> [Expr]) -> Expr -> [Expr]
counterExampleGeneralizations grounds e =
  [ canonicalize $ g
  | g <- fastCandidateGeneralizations isListable e
  , isCounterExample g
  ]
  where
  isListable = not . null . grounds . holeAsTypeOf
  isCounterExample  =  all (not . errorToFalse . eval False) . grounds
-- TODO: here I can maybe discardLater isInstanceOf to report multiple gens?
-- TODO: and since everything has the same format, I can at later stages score
--       gens and cgens in the same list?

-- |
-- Returns candidate generalizations for an expression.
-- (cf. 'candidateHoleGeneralizations')
--
-- This takes a function that returns whether to generalize a given
-- subexpression.
--
-- > > import Data.Haexpress.Fixtures
--
-- > > candidateGeneralizations (\e -> typ e == typ one) (one -+- two)
-- > [ _ :: Int
-- > , _ + _ :: Int
-- > , x + x :: Int
-- > , _ + 2 :: Int
-- > , 1 + _ :: Int
-- > ]
--
-- > > candidateGeneralizations (const True) (one -+- two)
-- > [ _ :: Int
-- > , _ _ :: Int
-- > , _ _ _ :: Int
-- > , _ x x :: Int
-- > , _ 1 _ :: Int
-- > , _ + _ :: Int
-- > , x + x :: Int
-- > , _ 2 :: Int
-- > , _ _ 2 :: Int
-- > , _ 1 2 :: Int
-- > , _ + 2 :: Int
-- > , 1 + _ :: Int
-- > ]
candidateGeneralizations :: (Expr -> Bool) -> Expr -> [Expr]
candidateGeneralizations should  =  map canonicalize
                                 .  fastCandidateGeneralizations should

-- |
-- Like 'candidateGeneralizations' but faster because result is not
-- canonicalized.  Variable names will be repeated across different types.
fastCandidateGeneralizations :: (Expr -> Bool) -> Expr -> [Expr]
fastCandidateGeneralizations should  =  concatMap fastCanonicalVariations
                                     .  candidateHoleGeneralizations should

-- |
-- Returns candidate generalizations for an expression by replacing values with
-- holes. (cf. 'candidateGeneralizations')
--
-- > > import Data.Haexpress.Fixtures
--
-- > > candidateHoleGeneralizations (\e -> typ e == typ one) (one -+- two)
-- > [ _ :: Int
-- > , _ + _ :: Int
-- > , _ + 2 :: Int
-- > , 1 + _ :: Int
-- > ]
--
-- > > candidateHoleGeneralizations (const True) (one -+- two)
-- > [ _ :: Int
-- > , _ _ :: Int
-- > , _ _ _ :: Int
-- > , _ 1 _ :: Int
-- > , _ + _ :: Int
-- > , _ 2 :: Int
-- > , _ _ 2 :: Int
-- > , _ 1 2 :: Int
-- > , _ + 2 :: Int
-- > , 1 + _ :: Int
-- > ]
candidateHoleGeneralizations :: (Expr -> Bool) -> Expr -> [Expr]
candidateHoleGeneralizations shouldGeneralize  =  gen
  where
  gen e@(e1 :$ e2)  =
    [holeAsTypeOf e | shouldGeneralize e]
    ++ productWith (:$) (gen e1) (gen e2)
    ++ map (:$ e2) (gen e1)
    ++ map (e1 :$) (gen e2)
  gen e
    | isVar e    =  []
    | otherwise  =  [holeAsTypeOf e | shouldGeneralize e]

productWith :: (a -> b -> c) -> [a] -> [b] -> [c]
productWith f xs ys = [f x y | x <- xs, y <- ys]
