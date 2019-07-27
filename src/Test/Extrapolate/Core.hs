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

  , backgroundOf

  , Option (..)
  , WithOption (..)
  , maxTests
  , extraInstances
  , maxConditionSize
  , groundsFor
  , namesFor
  , tBackground

  , counterExamples
  , counterExampleGen
  , counterExampleGens

  , generalizationsCEC

  , atoms
  , theoryAndReprExprs
  , theoryAndReprConds
  , candidateConditions
  , validConditions
  , weakestCondition
  , getBackground
  , fullInstances

  , Testable (..)
  , results
  , limitedResults

  , computeMinFailures
  )
where

import Data.List (sort)
import Data.Maybe
import Data.Ratio (Ratio, numerator, denominator)

import Data.Functor ((<$>)) -- for GHC <= 7.8
import Data.Monoid ((<>))

import Data.Typeable

import Test.LeanCheck.Utils
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

import Test.Speculate.Engine (theoryAndRepresentativesFromAtoms, classesFromSchemasAndVariables)
import Test.Speculate.Reason (Thy)
import Test.Speculate.Utils (boolTy, typesIn)

import Test.Extrapolate.Utils
import Test.Extrapolate.Expr
import Test.Extrapolate.Generalizable
import Test.Extrapolate.Generalization

getBackground :: Instances -> [Expr]
getBackground is = concat [eval err e | e@(Value "background" _) <- is]
  where
  err = error "Cannot evaluate background"

getEqInstancesFromBackground :: Instances -> Instances
getEqInstancesFromBackground is = eqs ++ iqs
  where
  eqs = [e | e@(Value "==" _) <- bg]
  iqs = [e | e@(Value "/=" _) <- bg]
  bg = getBackground is

backgroundOf :: Generalizable a => a -> [Expr]
backgroundOf x = getBackground $ instances x []

tBackground :: Testable a => a -> [Expr]
tBackground = getBackground . tinstances

-- I don't love Option/WithOption.  It is clever but it is not __clear__.
-- Maybe remove from future versions of the tool?
data Option = MaxTests Int
            | ExtraInstances Instances
            | MaxConditionSize Int
            | MinFailures (Ratio Int)
            | MaxSpeculateSize (Maybe Int)
            | ConstantBound (Maybe Int)
            | DepthBound (Maybe Int)
  deriving (Show, Typeable) -- Typeable needed for GHC <= 7.8

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

computeMaxSpeculateSize :: Testable a => a -> Maybe Int
computeMaxSpeculateSize p = head $ [b | MaxSpeculateSize b <- options p] ++ [Just 4]

computeConstantBound :: Testable a => a -> Maybe Int
computeConstantBound p = head $ [b | ConstantBound b <- options p] ++ [Nothing]

computeDepthBound :: Testable a => a -> Maybe Int
computeDepthBound p = head $ [b | DepthBound b <- options p] ++ [Nothing]

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
  (theoryAndReprConds p)
  (groundsFor p)
  (computeMinFailures p)

counterExampleGen :: Testable a => a -> Maybe (Expr,Maybe Expr)
counterExampleGen p  =  case counterExampleGens p of
  Nothing        -> Nothing
  Just (e,[])    -> Just (e,Nothing)
  Just (e,(g:_)) -> Just (e,Just g)

-- Generates expression schemas and a theory
theoryAndReprsFromPropAndAtoms :: Testable a => a -> [[Expr]] -> (Thy,[[Expr]])
theoryAndReprsFromPropAndAtoms p ess =
  theoryAndRepresentativesFromAtoms
    (maxConditionSize p) compareExpr (const True) (===) ess
  where
  -- the following uses of keep make Speculate run faster by defaulting to
  -- "these things are not equal" even in cases that they are.  Despite
  -- failing to detect some equalities, Speculte will still be useful as a
  -- generator of quasi-canonical expressions.
  e1 === e2 = keep e1 && keep e2 && equal is m e1 e2
  keep e = maybe True (\b -> length (consts e) <= b) (computeConstantBound p)
        && maybe True (\b ->          depth e  <= b) (computeDepthBound p)
        && maybe True (\b ->           size e  <= b) (computeMaxSpeculateSize p)
-- NOTE: MaxSpeculateSize here should not be confused with the size
-- considering sizes of atoms (as per tier enumeration), this regards only the
-- size in number of symbols
  is = fullInstances p
  m  = maxTests p
  compareExpr :: Expr -> Expr -> Ordering
  compareExpr = compareComplexity <> lexicompareBy cmp
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex (concat ess) e1 e2 <> e1 `compare` e2
-- NOTE: "concat ess" may be an infinite list.  This function assumes
-- that the symbols will appear on the list eventually for termination.  If I
-- am correct this ivariant is assured by the rest of the code.

-- tinstances including auto generated Eq instances (based on background)
fullInstances :: Testable a => a -> Instances
fullInstances p = is ++ getEqInstancesFromBackground is
  where
  is = tinstances p

-- Given a property, returns the atoms to be passed to Speculate
atoms :: Testable a => a -> [[Expr]]
atoms p = ([vs] \/)
        . foldr (\/) [esU]
        $ [ eval (error msg :: [[Expr]]) tiersE
          | tiersE@(Value "tiers" _) <- is ]
  where
  vs = sort . mapMaybe (maybeHoleOfTy is) . nubMergeMap (typesIn . typ) $ esU
  esU = getBackground is
  msg = "canditateConditions: wrong type, not [[Expr]]"
  is = tinstances p

theoryAndReprExprs :: Testable a => a -> (Thy,[Expr])
theoryAndReprExprs p =
    (\(thy,ess) -> (thy, concat $ take (maxConditionSize p) ess))
  . theoryAndReprsFromPropAndAtoms p
  $ atoms p

theoryAndReprConds :: Testable a => a -> (Thy, [Expr])
theoryAndReprConds p = (thy, filter (\c -> typ c == boolTy) es)
  where
  (thy,es) = theoryAndReprExprs p

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
