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
