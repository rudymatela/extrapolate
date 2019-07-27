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
  , module Test.Extrapolate.Speculation
  , module Test.Extrapolate.Generalizable
  , module Test.Extrapolate.Generalization
  , module Test.Extrapolate.ConditionalGeneralization
  , module Test.Extrapolate.Testable

  , counterExampleWithGeneralizations
  )
where

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

import Test.Speculate.Reason (Thy)
import Test.Speculate.Engine (classesFromSchemasAndVariables)

import Test.Extrapolate.Expr
import Test.Extrapolate.Speculation
import Test.Extrapolate.Generalizable
import Test.Extrapolate.Generalization
import Test.Extrapolate.ConditionalGeneralization
import Test.Extrapolate.Testable

counterExampleWithGeneralizations :: Testable a => a -> Maybe (Expr,[Expr])
counterExampleWithGeneralizations p  =  case counterExample p of
  Nothing -> Nothing
  Just e  -> Just (e,gens e ++ take 1 (cgens e))
  where
  grounds = testableGrounds p
  gens = counterExampleGeneralizations (testableGrounds p)
  cgens = conditionalCounterExampleGeneralizations
    (testableMaxConditionSize p)
    (testableAtoms p)
    (testableGrounds p)
    (testableMkEquation p)
