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

  -- * redefinitions of functions from Haexpress
  , canonicalizeWith
  , grounds
  , groundsAndBinds
  , vassignments -- TODO: rename to match Haexpress
  , vars -- TODO: rename to match Haexpress

  -- * re-exports from Speculate.Expr
  , isConstantNamed
  , isAssignment
  , preludeInstances
  , reifyListable
  , isListable
  , isListableT
  , maybeTiersE
  , equal
  , lexicompareBy -- TODO: remove?

  -- * to be removed:
  , nameWith -- TODO: remove
  )
where
-- TODO: avoid most of this module by using a single Expr to represent
--       counterexamples?
--
--       > val "prop" prop :$ val 0 :$ val 2
--
--       I'll just have to take care to avoid generalizing the prop.

import Data.Haexpress hiding (canonicalizeWith, vars, nubVars)
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
  , maybeTiersE
  , equal
  , lexicompareBy
  )
import qualified Test.Speculate.Expr as E (grounds, groundAndBinds)

import Data.Haexpress.Utils.Typeable (boolTy)

import Test.LeanCheck.Error (errorToFalse)
import Data.Typeable (Typeable)

type Exprs = [Expr]

nameWith :: Typeable a => String -> a -> Instances
nameWith = E.mkNameWith

canonicalizeWith :: Instances -> [Expr] -> [Expr]
canonicalizeWith is  =  unfold . c1 . unrepeatedToHole1 . fold
  where
  c1 e  =  e //- cn e
  cn e  =  E.canonicalizationWith (lookupNames is)
        $  fold [v | v <- E.vars e, not $ isHole v]

unrepeatedToHole1 :: Expr -> Expr
unrepeatedToHole1 e = e //- [(v, holeAsTypeOf v) | (v,1) <- countVars e]
  where
  countVars e = map (\e' -> (e',length . filter (== e') $ E.vars e)) $ E.nubVars e

grounds :: Instances -> [Expr] -> [ [Expr] ]
grounds is = map unfold . E.grounds is . fold

groundsAndBinds :: Instances -> [Expr] -> [(Binds,[Expr])]
groundsAndBinds is = map (mapSnd unfold) . E.groundAndBinds is . fold
  where
  mapSnd f (x,y) = (x,f y)

vassignments :: [Expr] -> [[Expr]]
vassignments = map unfold . canonicalVariations . fold

vars :: [Expr] -> [Expr]
vars = E.nubVars . fold

isAssignmentTest :: Instances -> Int -> Expr -> Bool
isAssignmentTest is m e | typ e /= boolTy = False
isAssignmentTest is m e = length rs > 1 && length (filter id rs) == 1
  where
  rs = [errorToFalse $ eval False e' | [e'] <- take m $ grounds is [e]]
