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
  , module Test.LeanCheck.Utils.TypeBinding
  , module Test.Extrapolate.Expr

  , Generalizable (..)
  , this
  , reifyBackground
  , mkBackground
  , (+++)
  , backgroundOf
  , mkEq1
  , mkEq2
  , mkEq3
  , mkEq4
  , mkOrd1
  , mkOrd2
  , mkOrd3
  , mkOrd4

  , Option (..)
  , WithOption (..)
  , maxTests
  , extraInstances
  , maxConditionSize
  , groundsFor
  , hasEq
  , (*==*)
  , (*/=*)
  , (*<=*)
  , (*<*)
  , tBackground

  , counterExamples
  , counterExampleGen
  , counterExampleGens

  , candidateGeneralizations
  , generalizationsCE
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
  )
where

import Test.Extrapolate.Utils
import Test.LeanCheck.Utils
import Test.LeanCheck.Utils.TypeBinding
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
import Data.Maybe
import Data.List (sort)
import Data.Functor ((<$>)) -- for GHC <= 7.8
import Test.Extrapolate.Expr
import Test.LeanCheck.Error (errorToFalse)
import Data.Ratio (Ratio, numerator, denominator)
import Test.Extrapolate.TypeBinding -- for Haddock
import Test.Speculate.Reason (Thy)
import Test.Speculate.Engine (theoryAndRepresentativesFromAtoms, classesFromSchemasAndVariables)
import Test.Speculate.Utils (boolTy, typesIn)
import Data.Monoid ((<>))

-- |
-- Extrapolate can generalize counter-examples of any types that are
-- 'Generalizable'.
--
-- The only required function is 'instances' functions.
--
-- The following example shows a datatype and its instance:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- > instance Generalizable a => Generalizable (Stack a) where
-- >   instances s = this s $ instances (argTy1of1 s)
--
-- To declare 'instances' it may be useful to use type binding
-- operators such as: 'argTy1of1', 'argTy1of2' and 'argTy2of2'.
--
-- Instances can be automatically derived using
-- 'Test.Extrapolate.Derive.deriveGeneralizable'
-- which will also automatically derivate
-- 'Listable', 'Express' and 'Name' when needed.
class (Listable a, Express a, Name a, Show a) => Generalizable a where
  -- | List of symbols allowed to appear in side-conditions.
  --   Defaults to @[]@.  See 'value'.
  background :: a -> [Expr]
  background _ = []

  -- | Computes a list of reified instances.  See 'this'.
  instances :: a -> Instances -> Instances


instance Generalizable () where
  instances u = this u id

instance Generalizable Bool where
  background p = reifyEq p
              ++ [ value "not" not ]
  instances p = this p id

instance Generalizable Int where
  background x = reifyEqOrd x
  instances x = this x id

instance Generalizable Integer where
  background x = reifyEqOrd x
  instances x = this x id

instance Generalizable Char where
  background c = reifyEqOrd c
  instances c = this c id

instance (Generalizable a) => Generalizable (Maybe a) where
  background mx  =  mkEq1  (maybeEq  ->:> mx)
                 ++ mkOrd1 (maybeOrd ->:> mx)
                 ++ [ value "Just" (Just ->: mx) ]
  instances mx  =  this mx $ instances (fromJust mx)

instance (Generalizable a, Generalizable b) => Generalizable (Either a b) where
  background exy  =  mkEq2  (eitherEq  ->>:> exy)
                  ++ mkOrd2 (eitherOrd ->>:> exy)
                  ++ [ value "Left"  (Left  ->: exy)
                     , value "Right" (Right ->: exy) ]
  instances exy  =  this exy $ instances (fromLeft  exy)
                             . instances (fromRight exy)

instance (Generalizable a, Generalizable b) => Generalizable (a,b) where
  background xy  =  mkEq2  (pairEq  ->>:> xy)
                 ++ mkOrd2 (pairOrd ->>:> xy)
  instances xy  =  this xy $ instances (fst xy)
                           . instances (snd xy)

instance (Generalizable a, Generalizable b, Generalizable c)
      => Generalizable (a,b,c) where
  background xyz  =  mkEq3  (tripleEq  ->>>:> xyz)
                  ++ mkOrd3 (tripleOrd ->>>:> xyz)
  instances xyz  =  this xyz $ instances x . instances y . instances z
                    where (x,y,z) = xyz

instance (Generalizable a, Generalizable b, Generalizable c, Generalizable d)
      => Generalizable (a,b,c,d) where
  background xyzw  =  mkEq4  (quadrupleEq  ->>>>:> xyzw)
                   ++ mkOrd4 (quadrupleOrd ->>>>:> xyzw)
  instances xyzw  =  this xyzw $ instances x
                               . instances y
                               . instances z
                               . instances w
                     where (x,y,z,w) = xyzw

instance Generalizable a => Generalizable [a] where
  background xs  =  mkEq1  (listEq  ->:> xs)
                 ++ mkOrd1 (listOrd ->:> xs)
                 ++ [ value "length" (length -:> xs) ]
                 ++ [ value "elem"      (elemBy (*==*) ->:> xs) | hasEq $ head xs ]
  instances xs  =  this xs $ instances (head xs)

instance Generalizable Ordering where
  background o  =  reifyEqOrd o
  instances o  =  this o id

mkEq1 :: (Generalizable a, Generalizable b)
          => ((b -> b -> Bool) -> a -> a -> Bool) -> [Expr]
mkEq1 makeEq = takeWhile (\_ -> hasEq x) . mkEq $ makeEq (*==*)
  where
  x = arg1 ==: makeEq

mkEq2 :: (Generalizable a, Generalizable b, Generalizable c)
          => ((b -> b -> Bool) -> (c -> c -> Bool) -> a -> a -> Bool) -> [Expr]
mkEq2 makeEq = takeWhile (\_ -> hasEq x && hasEq y) . mkEq
             $ makeEq (*==*) (*==*)
  where
  x = arg1 ==: makeEq
  y = arg2 ==: makeEq

mkEq3 :: (Generalizable a, Generalizable b, Generalizable c, Generalizable d)
          => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> a -> a -> Bool)
          -> [Expr]
mkEq3 makeEq = takeWhile (\_ -> hasEq x && hasEq y && hasEq z) . mkEq
             $ makeEq (*==*) (*==*) (*==*)
  where
  x = arg1 ==: makeEq
  y = arg2 ==: makeEq
  z = arg3 ==: makeEq

mkEq4 :: (Generalizable a, Generalizable b, Generalizable c, Generalizable d, Generalizable e)
          => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> (e->e->Bool) -> a -> a -> Bool)
          -> [Expr]
mkEq4 makeEq = takeWhile (\_ -> hasEq x && hasEq y && hasEq z && hasEq w) . mkEq
             $ makeEq (*==*) (*==*) (*==*) (*==*)
  where
  x = arg1 ==: makeEq
  y = arg2 ==: makeEq
  z = arg3 ==: makeEq
  w = arg4 ==: makeEq

mkOrd1 :: (Generalizable a, Generalizable b)
          => ((b -> b -> Bool) -> a -> a -> Bool) -> [Expr]
mkOrd1 makeOrd = takeWhile (\_ -> hasOrd x)
                   [ value "<=" (             makeOrd (*<=*))
                   , value "<"  (not .: flip (makeOrd (*<=*))) ]
  where
  x = arg1 ==: makeOrd

mkOrd2 :: (Generalizable a, Generalizable b, Generalizable c)
          => ((b -> b -> Bool) -> (c -> c -> Bool) -> a -> a -> Bool) -> [Expr]
mkOrd2 makeOrd = takeWhile (\_ -> hasOrd x && hasOrd y)
                   [ value "<=" (             makeOrd (*<=*) (*<=*))
                   , value "<"  (not .: flip (makeOrd (*<=*) (*<=*))) ]
  where
  x = arg1 ==: makeOrd
  y = arg2 ==: makeOrd

mkOrd3 :: (Generalizable a, Generalizable b, Generalizable c, Generalizable d)
          => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> a -> a -> Bool)
          -> [Expr]
mkOrd3 makeOrd = takeWhile (\_ -> hasOrd x && hasOrd y && hasOrd z)
                   [ value "<="              (makeOrd (*<=*) (*<=*) (*<=*))
                   , value "<"  (not .: flip (makeOrd (*<=*) (*<=*) (*<=*))) ]
  where
  x = arg1 ==: makeOrd
  y = arg2 ==: makeOrd
  z = arg3 ==: makeOrd

mkOrd4 :: (Generalizable a, Generalizable b, Generalizable c, Generalizable d, Generalizable e)
          => ((b->b->Bool) -> (c->c->Bool) -> (d->d->Bool) -> (e->e->Bool) -> a -> a -> Bool)
          -> [Expr]
mkOrd4 makeOrd = takeWhile (\_ -> hasOrd x && hasOrd y && hasOrd z && hasOrd w)
                   [ value "<="              (makeOrd (*<=*) (*<=*) (*<=*) (*<=*))
                   , value "<"  (not .: flip (makeOrd (*<=*) (*<=*) (*<=*) (*<=*))) ]
  where
  x = arg1 ==: makeOrd
  y = arg2 ==: makeOrd
  z = arg3 ==: makeOrd
  w = arg4 ==: makeOrd

-- | Usage: @ins "x" (undefined :: Type)@
ins :: Generalizable a => a -> Instances
ins x = reifyListable x
     ++ reifyName x
     ++ reifyBackground x

-- | Used in the definition of 'instances'
--   in 'Generalizable' typeclass instances.
this :: Generalizable a
     => a -> (Instances -> Instances) -> Instances -> Instances
this x f is =
  if isListableT is (typeOf x)
    then is
    else f (ins x ++ is)
-- TODO: change type to a -> [Instances -> Instances] -> Instances -> Instances

reifyBackground :: Generalizable a => a -> Instances
reifyBackground = mkBackground . background

mkBackground :: [Expr] -> Instances
mkBackground es = [value "background" es]

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

-- |  generalizes an expression by making it less defined,
--    starting with smaller changes, then bigger changes:
--
--    1: change value to variable
--    1.1: if a variable of the value type exists, use it
--    1.2: if not, introduce new variable
--    2: change a variable to a new variable
--
-- The above is the ideal, but let's start with a simpler algorithm:
--
--    1: change value to hole
candidateGeneralizations :: (Expr -> Bool) -> Expr -> [Expr]
candidateGeneralizations shouldGeneralize  =  gen
  where
  gen e@(e1 :$ e2)  =
    [holeAsTypeOf e | shouldGeneralize e]
    ++ productWith (:$) (gen e1) (gen e2)
    ++ map (:$ e2) (gen e1)
    ++ map (e1 :$) (gen e2)
  gen e
    | isVar e    =  []
    | otherwise  =  [holeAsTypeOf e | shouldGeneralize e]

productWith :: (a -> b -> c) -> [a] -> [b] -> [c]
productWith f xs ys = [f x y | x <- xs, y <- ys]

-- I don't love Option/WithOption.  It is clever but it is not __clear__.
-- Maybe remove from future versions of the tool?
data Option = MaxTests Int
            | ExtraInstances Instances
            | MaxConditionSize Int
            | MinFailures (Ratio Int)
            | MaxSpeculateSize (Maybe Int)
            | ConditionBound (Maybe Int)
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
groundsFor p  =  take (maxTests p) . grounds (tinstances p)

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

computeConditionBound :: Testable a => a -> Maybe Int
computeConditionBound p = head $ [b | ConditionBound b <- options p] ++ [Nothing]

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
  Just e  -> Just (e,generalizationsCE p e)

generalizationsCE :: Testable a => a -> Expr -> [Expr]
generalizationsCE p e =
  [ canonicalizeWith (namesFor p) g'
  | g <- candidateGeneralizations (isListableFor p) e
  , g' <- canonicalVariations g
  , isCounterExample (groundsFor p) g'
  ]

generalizationsCEC :: Testable a => a -> Expr -> [Expr]
generalizationsCEC p e | maxConditionSize p <= 0 = []
generalizationsCEC p e =
  [ canonicalizeWith (namesFor p) $ wc -==>- g'
  | g <- candidateGeneralizations (isListableFor p) e
  , g' <- canonicalVariations g
  , let thycs = theoryAndReprConds p
  , let wc = weakestCondition thycs p g'
  , wc /= value "False" False
  , wc /= value "True"  True
  ]

isCounterExample :: (Expr -> [Expr]) -> Expr -> Bool
isCounterExample grounds  =  all (not . errorToFalse . eval False) . grounds

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
  keep e = maybe True (\b -> length (consts e) <= b) constBound
        && maybe True (\b ->          depth e  <= b) depthBound
        && maybe True (\b ->           size e  <= b) (computeMaxSpeculateSize p)
-- NOTE: MaxSpeculateSize here should not be confused with the size
-- considering sizes of atoms (as per tier enumeration), this regards only the
-- size in number of symbols
  constBound = computeConstantBound p
  depthBound = computeDepthBound p
  is = fullInstances p
  m  = maxTests p
  compareExpr :: Expr -> Expr -> Ordering
  compareExpr = compareComplexity <> lexicompareBy cmp
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex (concat ess) e1 e2 <> e1 `compare` e2
-- NOTE: "concat $ atoms args" may be an infinite list.  This function assumes
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
  vs = sort . mapMaybe holeOfTy . nubMergeMap (typesIn . typ) $ esU
  esU = getBackground is
  msg = "canditateConditions: wrong type, not [[Expr]]"
  is = tinstances p
  holeOfTy t = holeAsTypeOf . head . concat <$> maybeTiersE (preludeInstances ++ is) t

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

validConditions :: Testable a => (Thy,[Expr]) -> a -> Expr -> [(Expr,Int)]
validConditions thyes p e =
  [ (c,n) | c <- candidateConditions grounds thyes e
          , (True,n) <- [isConditionalCounterExample grounds $ c -==>- e]
          , n > minFailures
          ] ++ [(expr False,0)]
  where
  minFailures = computeMinFailures p
  grounds = groundsFor p

weakestCondition :: Testable a => (Thy,[Expr]) -> a -> Expr -> Expr
weakestCondition thyes p e = fst
                           . maximumOn snd
                           . takeBound (computeConditionBound p)
                           $ validConditions thyes p e

isConditionalCounterExample :: (Expr -> [Expr]) -> Expr -> (Bool, Int)
isConditionalCounterExample grounds e  =  andLength
  [ not . errorToFalse $ eval False e'
  | e' <- grounds e
  , errorToFalse . eval False . fst $ unimply e'
  ]
  where
  andLength ps = (and ps, length ps)

fromBackgroundOf :: (Generalizable a, Typeable b) => String -> a -> Maybe b
fromBackgroundOf nm = listToMaybe
                    . mapMaybe evaluate
                    . filter (`isConstantNamed` nm)
                    . background

hasEq :: Generalizable a => a -> Bool
hasEq x = isJust $ "==" `fromBackgroundOf` x -: mayb (x >- x >- bool)

hasOrd :: Generalizable a => a -> Bool
hasOrd x = isJust $ "<=" `fromBackgroundOf` x -: mayb (x >- x >- bool)

(*==*) :: Generalizable a => a -> a -> Bool
x *==* y = x == y
  where
  (==) = fromMaybe (error "(*==*): no (==) operator in background")
       $ "==" `fromBackgroundOf` x

(*/=*) :: Generalizable a => a -> a -> Bool
x */=* y = x /= y
  where
  (/=) = fromMaybe (error "(*/=*): no (/=) operator in background")
       $ "/=" `fromBackgroundOf` x

(*<=*) :: Generalizable a => a -> a -> Bool
x *<=* y = x <= y
  where
  (<=) = fromMaybe (error "(*<=*): no (<=) operator in background")
       $ "<=" `fromBackgroundOf` x

(*<*) :: Generalizable a => a -> a -> Bool
x *<* y = x < y
  where
  (<) = fromMaybe (error "(*<*): no (<) operator in background")
       $ "<" `fromBackgroundOf` x
