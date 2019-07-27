{-# LANGUAGE DeriveDataTypeable #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Extrapolate.Expr
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This module re-exports functionality from
-- "Data.Haexpress" and "Test.Speculate.Expr"
-- along with some extra utilities.
module Test.Extrapolate.Expr
  ( module Data.Haexpress
  , module Test.Speculate.Expr

  -- * redefinitions of functions from Haexpress
  , canonicalize
  , canonicalizeWith

  -- * misc re-exports
  , (-==>-)
  , unimply

  -- * new functions
  , isAssignmentTest
  , replaceFun
  )
where

import Data.Haexpress          hiding (canonicalize, canonicalizeWith)
import Data.Haexpress.Fixtures hiding (canonicalize, canonicalizeWith)
import Test.Speculate.Expr     hiding (canonicalize, canonicalizeWith)
import Test.LeanCheck.Error    (errorToFalse)

canonicalize :: Expr -> Expr
canonicalize  =  canonicalizeWith (lookupNames preludeNameInstances)

canonicalizeWith :: (Expr -> [String]) -> Expr -> Expr
canonicalizeWith namesFor  =  c1 . unrepeatedToHole1
  where
  c1 e  =  e //- cn e
  cn e  =  canonicalizationWith namesFor
        $  fold [v | v <- vars e, not $ isHole v]

unrepeatedToHole1 :: Expr -> Expr
unrepeatedToHole1 e = e //- [(v, holeAsTypeOf v) | (v,1) <- countVars e]
  where
  countVars e = map (\e' -> (e',length . filter (== e') $ vars e)) $ nubVars e

isAssignmentTest :: (Expr -> [Expr]) -> Expr -> Bool
isAssignmentTest grounds e | typ e /= typ false = False
isAssignmentTest grounds e = length rs > 1 && length (filter id rs) == 1
  where
  rs = [errorToFalse $ eval False e' | e' <- grounds e]

-- | /O(n)/.
-- Replaces the function in the given 'Expr'.
--
-- > replaceFun timesE (plusE :$ one :$ two) = timesE :$ one :$ two
-- > replaceFun absE (idE :$ one) = absE :$ one
-- > replaceFun two (one) = two
replaceFun :: Expr -> Expr -> Expr
replaceFun ef e = foldApp (ef:tail es)
  where
  es = unfoldApp e

unimply :: Expr -> (Expr,Expr)
unimply ((op :$ e1) :$ e2) | op == implies  =  (e1,e2)
unimply _  =  error "unimply: not an implication"
