{-# LANGUAGE DeriveDataTypeable #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Extrapolate.Core
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This is the core of extrapolate.
module Test.Extrapolate.Core
  ( module Test.LeanCheck
  , module Test.Extrapolate.Expr
  , module Test.Extrapolate.Generalizable
  , module Test.Extrapolate.Generalization
  , module Test.Extrapolate.Testable
  , module Test.Extrapolate.Speculation

  , backgroundOf

  , counterExampleGen
  , counterExampleGens

  , generalizationsCEC

  , candidateConditions
  , validConditions
  , weakestCondition
  )
where

-- TODO: split this module into Testable, ConditionalGeneralization and
--       Speculation

import Data.Functor ((<$>)) -- for GHC <= 7.8
import Data.Monoid ((<>))

import Data.Typeable

import Test.LeanCheck hiding
  ( Testable
  , results
  , counterExamples
  , counterExample
  , productWith
  , check
  , checkFor
  , checkResult
  , checkResultFor
  )
import Test.LeanCheck.Error (errorToFalse)

import Test.Speculate.Reason (Thy)
import Test.Speculate.Engine (classesFromSchemasAndVariables)

import Test.Extrapolate.Expr
import Test.Extrapolate.Generalizable
import Test.Extrapolate.Generalization
import Test.Extrapolate.Speculation
import Test.Extrapolate.Testable
import Test.Extrapolate.Utils

backgroundOf :: Generalizable a => a -> [Expr]
backgroundOf x = concat [evl e | e@(Value "background" _) <- instances x []]
-- TODO: move backgroundOf to Test

counterExampleGens :: Testable a => a -> Maybe (Expr,[Expr])
counterExampleGens p  =  case counterExample p of
  Nothing -> Nothing
  Just e  -> Just (e,counterExampleGeneralizations (groundsFor p) e)

generalizationsCEC :: Testable a => a -> Expr -> [Expr]
generalizationsCEC p e | maxConditionSize p <= 0 = []
generalizationsCEC p e =
  [ canonicalize $ wc -==>- g
  | g <- fastCandidateGeneralizations (isListableFor p) e
  , let wc = weakestCondition g
  , wc /= value "False" False
  , wc /= value "True"  True
  ]
  where
  canonicalize = canonicalizeWith (namesFor p)
  weakestCondition = weakestConditionFor p

weakestConditionFor :: Testable a => a -> Expr -> Expr
weakestConditionFor p = weakestCondition
  (theoryAndReprConds (tinstances p) (maxTests p) (maxConditionSize p))
  (groundsFor p)
  (computeMinFailures p)

counterExampleGen :: Testable a => a -> Maybe (Expr,Maybe Expr)
counterExampleGen p  =  case counterExampleGens p of
  Nothing        -> Nothing
  Just (e,[])    -> Just (e,Nothing)
  Just (e,(g:_)) -> Just (e,Just g)

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
