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
  , atoms
  )
where

import Data.List
import Data.Maybe

import Test.LeanCheck ((\/))

import Test.Speculate.Engine (theoryAndRepresentativesFromAtoms)
import Test.Speculate.Reason (Thy)
import Test.Speculate.Utils (boolTy, typesIn)

import Test.Extrapolate.Expr
import Test.Extrapolate.Utils

-- Generates expression schemas and a theory
theoryAndReprsFromPropAndAtoms :: Instances -> Int -> Int -> [[Expr]] -> (Thy,[[Expr]])
theoryAndReprsFromPropAndAtoms is maxTests maxConditionSize ess =
  theoryAndRepresentativesFromAtoms
    maxConditionSize compareExpr (const True) (===) ess
  where
  e1 === e2 = isTrue gs $ mkEquation eqis e1 e2
  gs = take maxTests . grounds (lookupTiers $ is)
  eqis = getEqInstancesFromBackground is
  compareExpr :: Expr -> Expr -> Ordering
  compareExpr = compareComplexity <> lexicompareBy cmp
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex (concat ess) e1 e2 <> e1 `compare` e2
-- NOTE: "concat ess" may be an infinite list.  This function assumes
-- that the symbols will appear on the list eventually for termination.  If I
-- am correct this ivariant is assured by the rest of the code.

-- Given a property, returns the atoms to be passed to Speculate
atoms :: Instances -> [[Expr]]
atoms is = ([vs] \/)
         . foldr (\/) [esU]
         $ [ eval (error msg :: [[Expr]]) tiersE
           | tiersE@(Value "tiers" _) <- is ]
  where
  vs = sort . mapMaybe (maybeHoleOfTy is) . nubMergeMap (typesIn . typ) $ esU
  esU = getBackground is
  msg = "canditateConditions: wrong type, not [[Expr]]"

theoryAndReprExprs :: Instances -> Int -> Int -> (Thy,[Expr])
theoryAndReprExprs is maxTests maxConditionSize  =
    (\(thy,ess) -> (thy, concat $ take maxConditionSize ess))
  . theoryAndReprsFromPropAndAtoms is maxTests maxConditionSize
  $ atoms is

theoryAndReprConds :: Instances -> Int -> Int -> (Thy, [Expr])
theoryAndReprConds is maxTests maxConditionSize  = (thy, filter (\c -> typ c == boolTy) es)
  where
  (thy,es) = theoryAndReprExprs is maxTests maxConditionSize

getEqInstancesFromBackground :: Instances -> Instances
getEqInstancesFromBackground is = eqs ++ iqs
  where
  eqs = [e | e@(Value "==" _) <- bg]
  iqs = [e | e@(Value "/=" _) <- bg]
  bg = getBackground is

getBackground :: Instances -> [Expr]
getBackground is = concat [eval err e | e@(Value "background" _) <- is]
   where
   err = error "Cannot evaluate background"
