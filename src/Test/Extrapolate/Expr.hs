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
-- "Data.Express" and "Test.Speculate.Expr"
-- along with some extra utilities.
module Test.Extrapolate.Expr
  ( module Data.Express
  , module Test.Speculate.Expr

  , canonicalizeUsingHoles
  , canonicalizeUsingHolesWith
  , unand
  , replaceFun

  -- * misc re-exports
  , (-&&-)
  , false
  )
where

import Data.Express
import Data.Express.Fixtures
import Test.Speculate.Expr

-- |
-- Like 'canonicalize' but uses holes for unrepeated variables.
canonicalizeUsingHoles :: Expr -> Expr
canonicalizeUsingHoles  =  canonicalizeUsingHolesWith (lookupNames preludeNameInstances)

-- |
-- Like 'canonicalizeWith' but uses holes for unrepeated variables.
canonicalizeUsingHolesWith :: (Expr -> [String]) -> Expr -> Expr
canonicalizeUsingHolesWith namesFor  =  c1 . unrepeatedToHole1
  where
  c1 e  =  e //- cn e
  cn e  =  canonicalizationWith namesFor
        $  fold [v | v <- vars e, not $ isHole v]
  unrepeatedToHole1 e = e //- [(v, holeAsTypeOf v) | (v,1) <- countVars e]
  countVars e = map (\e' -> (e',length . filter (== e') $ vars e)) $ nubVars e

unand :: Expr -> (Expr,Expr)
unand ((op :$ e1) :$ e2) | op == andE  =  (e1,e2)
unand _  =  error "unimply: not an implication"

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
