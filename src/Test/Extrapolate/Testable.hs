-- |
-- Module      : Test.Extrapolate.Testable
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This defines the 'Testable' typeclass
-- and utilities involving it.
--
-- You are probably better off importing "Test.Extrapolate".
{-# LANGUAGE DeriveDataTypeable #-} -- for GHC <= 7.8
module Test.Extrapolate.Testable
  ( Testable (..)
  , results
  , limitedResults
  , counterExample
  , counterExamples

  , Option (..)
  , WithOption (..)

  , testableMaxTests
  , testableMaxConditionSize
  , testableExtraInstances

  , testableGrounds
  , testableNames

  , testableBackground
  , testableMkEquation
  , testableAtoms
  )
where

import Data.List
import Data.Maybe
import Data.Ratio (Ratio)

import Test.Extrapolate.Utils

import Test.LeanCheck hiding (Testable, results, counterExample, counterExamples)
import Test.LeanCheck.Utils (bool, int)

import Test.Extrapolate.Generalizable


class Typeable a => Testable a where
  resultiers :: a -> [[(Expr,Bool)]]
  tinstances :: a -> Instances
  options :: a -> Options
  options _ = []

instance Testable a => Testable (WithOption a) where
  resultiers (p `With` o) = resultiers p
  tinstances (p `With` o) = tinstances p ++ testableExtraInstances (p `With` o)
  options    (p `With` o) = o : options p

instance Testable Bool where
  resultiers p = [[(value "prop" p, p)]]
  tinstances _ = instances bool . instances int $ []

instance (Testable b, Generalizable a, Listable a) => Testable (a->b) where
  resultiers p = concatMapT resultiersFor tiers
    where resultiersFor x = mapFst (replaceFun (value "prop" p :$ expr x)) `mapT` resultiers (p x)
          mapFst f (x,y) = (f x, y)
  tinstances p = instances (undefarg p) $ tinstances (p undefined)
    where
    undefarg :: (a -> b) -> a
    undefarg _ = undefined


results :: Testable a => a -> [(Expr,Bool)]
results = concat . resultiers

limitedResults :: Testable a => a -> [(Expr,Bool)]
limitedResults p  =  take (testableMaxTests p) (results p)


counterExample :: Testable a => a -> Maybe Expr
counterExample  =  listToMaybe . counterExamples

counterExamples :: Testable a => a -> [Expr]
counterExamples p  =  [as | (as,False) <- limitedResults p]


-- I don't love Option/WithOption.  It is clever but it is not __clear__.
-- Maybe remove from future versions of the tool?
data Option = MaxTests Int
            | ExtraInstances Instances
            | MaxConditionSize Int
  deriving Typeable -- for GHC <= 7.8

data WithOption a = With
                  { property :: a
                  , option :: Option }
  deriving Typeable -- for GHC <= 7.8

type Options = [Option]


testableMaxTests :: Testable a => a -> Int
testableMaxTests p  =  head $ [m | MaxTests m <- options p] ++ [500]

testableMaxConditionSize :: Testable a => a -> Int
testableMaxConditionSize p  =  head $ [m | MaxConditionSize m <- options p] ++ [4]

testableExtraInstances :: Testable a => a -> Instances
testableExtraInstances p  =  concat [is | ExtraInstances is <- options p]


testableGrounds :: Testable a => a -> Expr -> [Expr]
testableGrounds p  =  take (testableMaxTests p) . grounds (lookupTiers $ tinstances p)

testableMkEquation :: Testable a => a -> Expr -> Expr -> Expr
testableMkEquation p  =  mkEquation (getEqInstancesFromBackground is)
  where
  is = tinstances p
  getEqInstancesFromBackground is = eqs ++ iqs
    where
    eqs = [e | e@(Value "==" _) <- bg]
    iqs = [e | e@(Value "/=" _) <- bg]
    bg = concat [evl e | e@(Value "background" _) <- is]

testableNames :: Testable a => a -> Expr -> [String]
testableNames  =  lookupNames . tinstances

testableBackground :: Testable a => a -> [Expr]
testableBackground p  =  concat [eval err e | e@(Value "background" _) <- tinstances p]
  where
  err = error "Cannot evaluate background"

-- Given a property, returns the atoms to be passed to Speculate
testableAtoms :: Testable a => a -> [[Expr]]
testableAtoms  =  atoms . tinstances
  where
  atoms :: Instances -> [[Expr]]
  atoms is = ([vs] \/)
           . foldr (\/) [esU]
           $ [ eval (error msg :: [[Expr]]) tiersE
             | tiersE@(Value "tiers" _) <- is ]
    where
    vs = sort . mapMaybe (maybeHoleOfTy is) . nubMergeMap (typesIn . typ) $ esU
    esU = concat [evl e | e@(Value "background" _) <- is]
    msg = "canditateConditions: wrong type, not [[Expr]]"
