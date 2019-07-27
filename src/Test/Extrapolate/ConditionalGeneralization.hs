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

import Test.Extrapolate.Testable -- TODO: remove me

conditionalCounterExampleGeneralizations :: Testable a => a -> Expr -> [Expr]
conditionalCounterExampleGeneralizations p e | maxConditionSize p <= 0 = []
conditionalCounterExampleGeneralizations p e =
  [ canonicalize $ wc -==>- g
  | g <- fastCandidateGeneralizations (isListableFor p) e
  , let wc = weakestCondition' g
  , wc /= value "False" False
  , wc /= value "True"  True
  ]
  where
  canonicalize = canonicalizeWith (namesFor p)
  weakestCondition' = weakestCondition
    (theoryAndReprConds (tinstances p) (maxConditionSize p) (===))
    grounds
  e1 === e2 = isTrue grounds $ e1 -==- e2
  grounds = groundsFor p
  (-==-) = mkEquationFor p

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

validConditions :: (Thy,[Expr]) -> (Expr -> [Expr]) -> Expr -> [(Expr,Ratio Int)]
validConditions thyes grounds e =
  [ (c,n) | c <- candidateConditions grounds thyes e
          , (True,n) <- [isConditionalCounterExample grounds $ c -==>- e]
          ] ++ [(expr False,0)]

weakestCondition :: (Thy,[Expr]) -> (Expr -> [Expr]) -> Expr -> Expr
weakestCondition thyes grounds  =
  fst . maximumOn snd . validConditions thyes grounds

isConditionalCounterExample :: (Expr -> [Expr]) -> Expr -> (Bool, Ratio Int)
isConditionalCounterExample grounds e  =  andLength
  [ not . errorToFalse $ eval False e'
  | e' <- gs
  , errorToFalse . eval False . fst $ unimply e'
  ]
  where
  gs = grounds e
  andLength ps = (and ps, length ps % length gs)

-- Here we use Ratio Int instead of simply an Int because the number of tests
-- for a conditional generalization may vary for finite types.
