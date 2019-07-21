{-# LANGUAGE DeriveDataTypeable #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Extrapolate.IO
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This module re-exports functionality from
-- "Data.Haexpress" and "Test.Speculate.Expr",
-- but instead of working on single expressions
-- it works on lists of expressions
-- (the choosen representation for counter-examples).
module Test.Extrapolate.Exprs
  ( module Data.Haexpress

  -- * types
  , Exprs
  , Binds
  , Instances

  -- * new functions
  , isAssignmentTest
  , replaceFun

  -- * redefinitions of functions from Haexpress
  , canonicalizeWith

  -- * re-exports from Speculate.Expr
  , isConstantNamed
  , isAssignment
  , preludeInstances
  , reifyListable
  , isListable
  , isListableT
  , tiersE
  , maybeTiersE
  , equal
  , lexicompareBy
  , grounds
  , groundAndBinds
  )
where

import Data.Haexpress hiding (canonicalizeWith)
import qualified Data.Haexpress as E

import Test.Speculate.Expr
  ( Binds
  , Instances
  , isConstantNamed
  , isAssignment
  , preludeInstances
  , reifyListable
  , isListable
  , isListableT
  , tiersE
  , maybeTiersE
  , equal
  , lexicompareBy
  , grounds
  , groundAndBinds
  )

import Data.Haexpress.Utils.Typeable (boolTy)

import Test.LeanCheck.Error (errorToFalse)

type Exprs = [Expr]

canonicalizeWith :: (Expr -> [String]) -> Expr -> Expr
canonicalizeWith namesFor  =  c1 . unrepeatedToHole1
  where
  c1 e  =  e //- cn e
  cn e  =  E.canonicalizationWith namesFor
        $  fold [v | v <- E.vars e, not $ isHole v]

unrepeatedToHole1 :: Expr -> Expr
unrepeatedToHole1 e = e //- [(v, holeAsTypeOf v) | (v,1) <- countVars e]
  where
  countVars e = map (\e' -> (e',length . filter (== e') $ E.vars e)) $ E.nubVars e

isAssignmentTest :: Instances -> Int -> Expr -> Bool
isAssignmentTest is m e | typ e /= boolTy = False
isAssignmentTest is m e = length rs > 1 && length (filter id rs) == 1
  where
  rs = [errorToFalse $ eval False e' | e' <- take m $ grounds is e]

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
