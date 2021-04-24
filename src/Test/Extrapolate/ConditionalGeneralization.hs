-- |
-- Module      : Test.Extrapolate.ConditionalGeneralization
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This defines utilities for conditional generalization.
--
-- You are probably better off importing "Test.Extrapolate".
module Test.Extrapolate.ConditionalGeneralization
  ( conditionalCounterExampleGeneralizations
  , validConditions
  , candidateConditions
  )
where

import Data.Ratio

import Test.LeanCheck.Error (errorToFalse)

import Test.Extrapolate.Speculation
import Test.Extrapolate.Generalization
import Test.Extrapolate.Utils

conditionalCounterExampleGeneralizations
  :: Int -> [[Expr]] -> (Expr -> [Expr]) -> (Expr -> Expr -> Expr)
  -> Expr -> [Expr]
conditionalCounterExampleGeneralizations maxConditionSize atoms grounds (-==-) e  =
  [ canonicalize $ g -&&- wc
  | g <- fastCandidateGeneralizations isListable e
  , let wc = weakestCondition' g
  , wc /= value "False" False
  , wc /= value "True"  True
  ]
  where
  isListable = not . null . grounds . holeAsTypeOf
  weakestCondition' = weakestCondition
    (theoryAndReprConds (===) maxConditionSize atoms)
    grounds
  e1 === e2 = isTrue grounds $ e1 -==- e2

candidateConditions :: (Expr -> [Expr]) -> (Thy,[Expr]) -> Expr -> [Expr]
candidateConditions grounds (thy,cs) e = expr True :
  [ c | (c,_) <- classesFromSchemasAndVariables thy (nubVars e) cs
      , hasVar c
      , not (isAssignment c)
      , not (isAssignmentTest grounds c)
      ]
-- 'expr True' is expected by the functions that call candidateConditions.  It
-- is always useful to check if a generalization without any conditions still
-- passes (that means we should skip as there is an already reported
-- unconditional generalization).

validConditions :: (Thy,[Expr]) -> (Expr -> [Expr]) -> Expr -> [Expr]
validConditions thyes grounds e =
  [ c | c <- candidateConditions grounds thyes e
      , isCounterExample $ e -&&- c ] ++ [expr False]
  where
  isCounterExample  =  all (not . errorToFalse . eval False) . grounds

weakestCondition :: (Thy,[Expr]) -> (Expr -> [Expr]) -> Expr -> Expr
weakestCondition thyes grounds e  =
  maximumOn (ratioFailures grounds . (e -&&-)) $ validConditions thyes grounds e

ratioFailures :: (Expr -> [Expr]) -> Expr -> Ratio Int
ratioFailures grounds e  =  length ps % length gs
  where
  gs = grounds e
  ps = filter (errorToFalse . eval False . snd . unand) gs

isAssignmentTest :: (Expr -> [Expr]) -> Expr -> Bool
isAssignmentTest grounds e | typ e /= typ false = False
isAssignmentTest grounds e = length rs > 1 && length (filter id rs) == 1
  where
  rs = [errorToFalse $ eval False e' | e' <- grounds e]
