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
  ( generalizationsCEC
  , equalsFor
  , validConditions
  , candidateConditions
  )
where

import Test.LeanCheck.Error (errorToFalse)

import Test.Extrapolate.Speculation
import Test.Extrapolate.Generalization
import Test.Extrapolate.Utils

import Test.Extrapolate.Testable -- TODO: remove me

generalizationsCEC :: Testable a => a -> Expr -> [Expr]
generalizationsCEC p e | maxConditionSize p <= 0 = []
generalizationsCEC p e =
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
    (groundsFor p)
    (computeMinFailures p)
  (===) = equalsFor p

equalsFor :: Testable a => a -> Expr -> Expr -> Bool
equalsFor p = (===)
  where
  e1 === e2 = isTrue gs $ mkEquationFor p e1 e2
  gs = take (maxTests p) . grounds (lookupTiers $ tinstances p)

mkEquationFor :: Testable a => a -> Expr -> Expr -> Expr
mkEquationFor p = mkEquation (getEqInstancesFromBackground is)
  where
  is = tinstances p



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

validConditions :: (Thy,[Expr]) -> (Expr -> [Expr]) -> Int -> Expr -> [(Expr,Int)]
validConditions thyes grounds minFailures e =
  [ (c,n) | c <- candidateConditions grounds thyes e
          , (True,n) <- [isConditionalCounterExample grounds $ c -==>- e]
          , n > minFailures
          ] ++ [(expr False,0)]

weakestCondition :: (Thy,[Expr]) -> (Expr -> [Expr]) -> Int -> Expr -> Expr
weakestCondition thyes grounds minFailures =
  fst . maximumOn snd . validConditions thyes grounds minFailures

isConditionalCounterExample :: (Expr -> [Expr]) -> Expr -> (Bool, Int)
isConditionalCounterExample grounds e  =  andLength
  [ not . errorToFalse $ eval False e'
  | e' <- grounds e
  , errorToFalse . eval False . fst $ unimply e'
  ]
  where
  andLength ps = (and ps, length ps)

getEqInstancesFromBackground :: Instances -> Instances
getEqInstancesFromBackground is = eqs ++ iqs
  where
  eqs = [e | e@(Value "==" _) <- bg]
  iqs = [e | e@(Value "/=" _) <- bg]
  bg = concat [evl e | e@(Value "background" _) <- is]
