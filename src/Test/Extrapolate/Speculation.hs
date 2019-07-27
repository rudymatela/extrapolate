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

import Test.LeanCheck ((\/))

import Test.Speculate.Engine (theoryAndRepresentativesFromAtoms, classesFromSchemasAndVariables)
import Test.Speculate.Reason (Thy)
import Test.Speculate.Utils (boolTy, typesIn)

import Test.Extrapolate.Utils
import Test.Extrapolate.Expr

-- Generates expression schemas and a theory
theoryAndReprsFromPropAndAtoms :: Int -> (Expr -> Expr -> Bool) -> [[Expr]] -> (Thy,[[Expr]])
theoryAndReprsFromPropAndAtoms maxConditionSize (===) ess =
  theoryAndRepresentativesFromAtoms
    maxConditionSize compareExpr (const True) (===) ess
  where
  compareExpr :: Expr -> Expr -> Ordering
  compareExpr = compareComplexity <> lexicompareBy cmp
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex (concat ess) e1 e2 <> e1 `compare` e2
-- NOTE: "concat ess" may be an infinite list.  This function assumes
-- that the symbols will appear on the list eventually for termination.  If I
-- am correct this ivariant is assured by the rest of the code.

theoryAndReprExprs :: Int -> (Expr -> Expr -> Bool) -> [[Expr]] -> (Thy,[Expr])
theoryAndReprExprs maxConditionSize (===) =
    (\(thy,ess) -> (thy, concat $ take maxConditionSize ess))
  . theoryAndReprsFromPropAndAtoms maxConditionSize (===)

theoryAndReprConds :: Int -> (Expr -> Expr -> Bool) -> [[Expr]] -> (Thy, [Expr])
theoryAndReprConds maxConditionSize (===) ess  =  (thy, filter (\c -> typ c == boolTy) es)
  where
  (thy,es) = theoryAndReprExprs maxConditionSize (===) ess
