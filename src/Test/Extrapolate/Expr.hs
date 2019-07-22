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
  , canonicalizeWith

  -- * new functions
  , isAssignmentTest
  , replaceFun
  )
where

import Data.Haexpress hiding (canonicalizeWith)
import qualified Data.Haexpress as E (canonicalizeWith)
import Test.Speculate.Expr hiding (Data.Haexpress, canonicalizeWith)
import Data.Haexpress.Utils.Typeable (boolTy)
import Test.LeanCheck.Error (errorToFalse)

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

isAssignmentTest :: (Expr -> [Expr]) -> Expr -> Bool
isAssignmentTest grounds e | typ e /= boolTy = False
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
