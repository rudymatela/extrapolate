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
module Test.Extrapolate.Testable
  ( Testable (..)
  , results
  , limitedResults
  , counterExample
  , counterExamples

  , Option (..)
  , WithOption (..)
  , maxTests
  , extraInstances
  , maxConditionSize
  , groundsFor
  , namesFor
  , isListableFor
  , tBackground
  , getBackground
  , computeMinFailures
  )
where

import Data.Maybe (listToMaybe)
import Data.Ratio (Ratio, numerator, denominator)

import Test.LeanCheck hiding (Testable, results, counterExample, counterExamples)
import Test.LeanCheck.Utils (bool, int)

import Test.Extrapolate.Generalizable

-- TODO: standardize the names of functions in this module
-- maybe: testableMaxTests, testableGrounds, etc...?

-- I don't love Option/WithOption.  It is clever but it is not __clear__.
-- Maybe remove from future versions of the tool?
data Option = MaxTests Int
            | ExtraInstances Instances
            | MaxConditionSize Int
            | MinFailures (Ratio Int)
  deriving Typeable -- Typeable needed for GHC <= 7.8

data WithOption a = With
                  { property :: a
                  , option :: Option }

type Options = [Option]

maxTests :: Testable a => a -> Int
maxTests p = head $ [m | MaxTests m <- options p] ++ [500]

extraInstances :: Testable a => a -> Instances
extraInstances p = concat [is | ExtraInstances is <- options p]

maxConditionSize :: Testable a => a -> Int
maxConditionSize p = head $ [m | MaxConditionSize m <- options p] ++ [4]

groundsFor :: Testable a => a -> Expr -> [Expr]
groundsFor p  =  take (maxTests p) . grounds (lookupTiers $ tinstances p)

isListableFor :: Testable a => a -> Expr -> Bool
isListableFor p e
  | e == value "prop" p  =  False
  | otherwise            =  isListable is e
  where
  is = tinstances p

namesFor :: Testable a => a -> Expr -> [String]
namesFor  =  lookupNames . tinstances

-- minimum number of failures for a conditional generalization
computeMinFailures :: Testable a => a -> Int
computeMinFailures p = max 2 $ m * numerator r `div` denominator r
  where
  r = head $ [r | MinFailures r <- options p] ++ [0]
  m = maxTests p

class Typeable a => Testable a where
  resultiers :: a -> [[(Expr,Bool)]]
  tinstances :: a -> Instances
  options :: a -> Options
  options _ = []

instance Testable a => Testable (WithOption a) where
  resultiers (p `With` o) = resultiers p
  tinstances (p `With` o) = tinstances p ++ extraInstances (p `With` o)
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
limitedResults p  =  take (maxTests p) (results p)

counterExamples :: Testable a => a -> [Expr]
counterExamples p  =  [as | (as,False) <- limitedResults p]

counterExample :: Testable a => a -> Maybe Expr
counterExample  =  listToMaybe . counterExamples

-- move this out'a here or localize
getBackground :: Instances -> [Expr]
getBackground is = concat [eval err e | e@(Value "background" _) <- is]
  where
  err = error "Cannot evaluate background"

tBackground :: Testable a => a -> [Expr]
tBackground = getBackground . tinstances
