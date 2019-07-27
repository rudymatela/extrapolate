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

  , canonicalizeUsingHoles
  , canonicalizeUsingHolesWith
  , unimply

  -- * misc re-exports
  , (-==>-)
  , (-&&-)
  , false
  , implies
  )
where

import Data.Haexpress
import Data.Haexpress.Fixtures
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

unimply :: Expr -> (Expr,Expr)
unimply ((op :$ e1) :$ e2) | op == implies  =  (e1,e2)
unimply _  =  error "unimply: not an implication"
