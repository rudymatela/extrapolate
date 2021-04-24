-- |
-- Module      : Test.Extrapolate.Speculation
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This defines utilities for speculation about side conditions.
--
-- You are probably better off importing "Test.Extrapolate".
module Test.Extrapolate.Speculation
  ( theoryAndReprConds

  -- re-exports from Speculate
  , Thy
  , Expr
  , classesFromSchemasAndVariables
  )
where

import Data.Monoid ((<>)) -- for GHC <= 8.2

import Test.LeanCheck ((\/))

import Test.Speculate.Engine (theoryAndRepresentativesFromAtoms, classesFromSchemasAndVariables)
import Test.Speculate.Reason (Thy)

import Test.Extrapolate.Utils
import Test.Extrapolate.Expr

theoryAndReprExprs :: (Expr -> Expr -> Bool) -> Int -> [[Expr]] -> (Thy,[Expr])
theoryAndReprExprs (===) maxConditionSize  =
    (\(thy,ess) -> (thy, concat $ take maxConditionSize ess))
  . theoryAndRepresentativesFromAtoms (===) maxConditionSize

theoryAndReprConds :: (Expr -> Expr -> Bool) -> Int -> [[Expr]] -> (Thy, [Expr])
theoryAndReprConds (===) maxConditionSize ess  =  (thy, filter (\c -> typ c == boolTy) es)
  where
  (thy,es) = theoryAndReprExprs (===) maxConditionSize ess
