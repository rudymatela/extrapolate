-- |
-- Module      : Test.Extrapolate
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Extrapolate is a property-based testing library capable of reporting
-- generalized counter-examples.
--
-- Consider the following faulty implementation of sort:
--
-- > sort :: Ord a => [a] -> [a]
-- > sort []      =  []
-- > sort (x:xs)  =  sort (filter (< x) xs)
-- >              ++ [x]
-- >              ++ sort (filter (> x) xs)
--
-- When tests pass, Extrapolate works like a regular property-based testing
-- library.  See:
--
-- > > check $ \xs -> sort (sort xs :: [Int]) == sort xs
-- > +++ OK, passed 360 tests.
--
-- When tests fail, Extrapolate reports a fully defined counter-example and a
-- generalization of failing inputs.  See:
--
-- > > > check $ \xs -> length (sort xs :: [Int]) == length xs
-- > *** Failed! Falsifiable (after 3 tests):
-- > [0,0]
-- >
-- > Generalization:
-- > x:x:_
--
-- The property fails for any integer @x@ and for any list @_@ at the tail.
module Test.Extrapolate
  (
-- * Checking properties
    check
  , checkResult
  , for
  , withBackground
  , withConditionSize
  , minFailures

-- * Generalizable types
  , Generalizable (..)
  , instances
  , Expr (..)
  , value, val
  , reifyEq
  , reifyOrd
  , reifyEqOrd

-- * Testable properties
  , Testable

-- ** Automatically deriving Generalizable instances
  , deriveGeneralizable
  , deriveGeneralizableIfNeeded
  , deriveGeneralizableCascading

-- * Typeclasses required by Generalizable
  , Name (..)
  , Express (..)

-- * Other useful modules
  , module Test.Extrapolate.TypeBinding
  , module Test.LeanCheck
  )
where

import Test.Extrapolate.Core
import Test.Extrapolate.Generalizable.Derive
import Test.Extrapolate.TypeBinding
import Test.Extrapolate.IO

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
