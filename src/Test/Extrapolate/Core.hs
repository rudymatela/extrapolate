{-# LANGUAGE DeriveDataTypeable #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Extrapolate.Core
-- Copyright   : (c) 2017 Rudy Matela
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
  , module Test.Extrapolate.Exprs

  , Generalizable (..)
  , this
  , backgroundWith
  , (+++)
  , backgroundOf
  , bgEq
  , bgOrd
  , bgEqWith1
  , bgEqWith2

  , Option (..)
  , WithOption (..)
  , maxTests
  , extraInstances
  , maxConditionSize
  , hasEq
  , (*==*)
  , (*/=*)
  , (*<=*)
  , (*<*)

  , counterExamples
  , counterExampleGen
  , counterExampleGens

  , generalizations
  , generalizationsCE
  , generalizationsCEC
  , generalizationsCounts

  , weakestCondition

  , matchList
  , newMatches

  , Testable (..)
  , results

  , areInstancesOf
  )
where

import Test.Extrapolate.Utils
import Test.LeanCheck.Utils
import Test.LeanCheck.Utils.TypeBinding
import Data.Typeable
import Data.Dynamic
import Test.LeanCheck hiding
  ( Testable (..)
  , results
  , counterExamples
  , counterExample
  , productWith
  , check
  , checkFor
  , checkResult
  , checkResultFor
  )
import Test.Extrapolate.Exprs (fold, unfold)
import Data.Maybe (listToMaybe, fromJust, fromMaybe, isJust, listToMaybe, catMaybes)
import Data.Either (isRight)
import Data.List (insert, nub, sort)
import Data.Functor ((<$>)) -- for GHC <= 7.8
import Test.Extrapolate.Exprs
import Test.LeanCheck.Error (errorToFalse)
import Data.Ratio (Ratio, numerator, denominator)
import Test.Extrapolate.TypeBinding -- for Haddock
import Test.Speculate.Reason (Thy)
import Test.Speculate.Engine (theoryAndRepresentativesFromAtoms, classesFromSchemasAndVariables)

-- | Extrapolate can generalize counter-examples of any types that are
--   'Generalizable'.
--
-- The core (and only required functions) of the generalizable typeclass are
-- the 'expr' and 'instances' functions.
--
-- The following example shows a datatype and its instance:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- > instance Generalizable a => Generalizable (Stack a) where
-- > name _ = "s"
-- > expr s@(Stack x y) = constant "Stack" (Stack ->>: s) :$ expr x :$ expr y
-- > expr s@Empty       = constant "Empty" (Empty   -: s)
-- > instances s = this s $ instances (argTy1of1 s)
--
-- To declare 'instances' and 'expr' it may be useful to use:
--
-- * LeanCheck's "Test.LeanCheck.Utils.TypeBinding" operators:
--   '-:', '->:', '->>:', ...;
-- * Extrapolate's "Test.Extrapolate.TypeBinding" operators:
--   'argTy1of1', 'argTy1of2', 'argTy2of2', ....
class (Listable a, Typeable a, Show a) => Generalizable a where
  -- | Transforms a value into an manipulable expression tree.
  --   See 'constant' and ':$'.
  expr :: a -> Expr

  -- | Common name for a variable, defaults to @"x"@.
  name :: a -> String
  name _ = "x"

  -- | List of symbols allowed to appear in side-conditions.
  --   Defaults to @[]@.  See 'constant'.
  background :: a -> [Expr]
  background _ = []

  -- | Computes a list of reified instances.  See 'this'.
  instances :: a -> Instances -> Instances


instance Generalizable () where
  expr = showConstant
  name _ = "u"
  instances u = this u id

instance Generalizable Bool where
  expr = showConstant
  name _ = "p"
  background p = bgEq p
              ++ [ constant "not" not ]
  instances p = this p id

instance Generalizable Int where
  expr = showConstant
  name _ = "x"
  background x = bgOrd x
  instances x = this x id

instance Generalizable Integer where
  expr = showConstant
  name _ = "x"
  background x = bgOrd x
  instances x = this x id

instance Generalizable Char where
  expr = showConstant
  name _ = "c"
  background c = bgOrd c
  instances c = this c id

instance (Generalizable a) => Generalizable (Maybe a) where
  expr mx@Nothing   =  constant "Nothing" (Nothing -: mx)
  expr mx@(Just x)  =  constant "Just"    (Just   ->: mx) :$ expr x
  name mx = "m" ++ name (fromJust mx)
  background mx  =  [ constant "Just" (Just ->: mx) ]
                 ++ bgEqWith1  (maybeEq  ->:> mx)
                 ++ bgOrdWith1 (maybeOrd ->:> mx)
  instances mx  =  this mx $ instances (fromJust mx)

instance (Generalizable a, Generalizable b) => Generalizable (Either a b) where
  expr lx@(Left x)   =  constant "Left"  (Left  ->: lx) :$ expr x
  expr ry@(Right y)  =  constant "Right" (Right ->: ry) :$ expr y
  name exy = "e" ++ name (fromLeft exy) ++ name (fromRight exy)
  background exy  =  [ constant "Left"  (Left  ->: exy)
                     , constant "Right" (Right ->: exy) ]
                  ++ bgEqWith2  (eitherEq  ->>:> exy)
                  ++ bgOrdWith2 (eitherOrd ->>:> exy)
  instances exy  =  this exy $ instances (fromLeft  exy)
                             . instances (fromRight exy)

instance (Generalizable a, Generalizable b) => Generalizable (a,b) where
  name xy  =  name (fst xy) ++ name (snd xy)
  expr (x,y)  =  constant "," ((,) ->>: (x,y))
              :$ expr x :$ expr y
  background xy  =  bgEqWith2  (pairEq  ->>:> xy)
                 ++ bgOrdWith2 (pairOrd ->>:> xy)
  instances xy  =  this xy $ instances (fst xy)
                           . instances (snd xy)

instance (Generalizable a, Generalizable b, Generalizable c)
      => Generalizable (a,b,c) where
  name xyz  =  name x ++ name y ++ name z
               where  (x,y,z) = xyz
  expr (x,y,z)  =  constant ",," ((,,) ->>>: (x,y,z))
                :$ expr x :$ expr y :$ expr z
  instances xyz  =  this xyz $ instances x . instances y . instances z
                    where (x,y,z) = xyz

instance (Generalizable a, Generalizable b, Generalizable c, Generalizable d)
      => Generalizable (a,b,c,d) where
  name xyzw  =  name x ++ name y ++ name z ++ name w
                where (x,y,z,w) = xyzw
  expr (x,y,z,w)  =  constant ",,," ((,,,) ->>>>: (x,y,z,w))
                  :$ expr x :$ expr y :$ expr z :$ expr w
  instances xyzw  =  this xyzw $ instances x
                               . instances y
                               . instances z
                               . instances w
                     where (x,y,z,w) = xyzw

instance Generalizable a => Generalizable [a] where
  name xs  =  name (head xs) ++ "s"
  expr (xs@[])      =  showConstant  ([]    -: xs)
  expr (xs@(y:ys))  =  constant ":"  ((:) ->>: xs) :$ expr y :$ expr ys
  background xs  =  [ constant "length" (length -:> xs) ]
                 ++ [ constant "elem"      (elemBy (*==*) ->:> xs) | hasEq $ head xs ]
                 ++ bgEqWith1  (listEq  ->:> xs)
                 ++ bgOrdWith1 (listOrd ->:> xs)
  instances xs  =  this xs $ instances (head xs)

instance Generalizable Ordering where
  name o  =  "o"
  expr o  =  showConstant o
  background o  =  bgOrd o
  instances o  =  this o id

bgEq :: (Eq a, Generalizable a) => a -> [Expr]
bgEq x = [ constant "==" ((==) -:> x)
         , constant "/=" ((/=) -:> x) ]

bgOrd :: (Ord a, Generalizable a) => a -> [Expr]
bgOrd x = [ constant "==" ((==) -:> x)
          , constant "/=" ((/=) -:> x)
          , constant "<"  ((<)  -:> x)
          , constant "<=" ((<=) -:> x) ]

bgEqWith1 :: (Generalizable a, Generalizable b)
          => ((b -> b -> Bool) -> a -> a -> Bool) -> [Expr]
bgEqWith1 makeEq = takeWhile (\_ -> hasEq x)
                 [ constant "==" (       makeEq (*==*))
                 , constant "/=" (not .: makeEq (*==*)) ]
  where
  x = argTy1of2 $ argTy1of2 makeEq

bgEqWith2 :: (Generalizable a, Generalizable b, Generalizable c)
          => ((b -> b -> Bool) -> (c -> c -> Bool) -> a -> a -> Bool) -> [Expr]
bgEqWith2 makeEq = takeWhile (\_ -> hasEq x && hasEq y)
                 [ constant "==" (       makeEq (*==*) (*==*))
                 , constant "/=" (not .: makeEq (*==*) (*==*)) ]
  where
  x = argTy1of2 $ argTy1of2 makeEq
  y = argTy1of2 . argTy1of2 $ argTy2of2 makeEq

bgOrdWith1 :: (Generalizable a, Generalizable b)
          => ((b -> b -> Bool) -> a -> a -> Bool) -> [Expr]
bgOrdWith1 makeOrd = takeWhile (\_ -> hasOrd x)
                   [ constant "<=" (             makeOrd (*<=*))
                   , constant "<"  (not .: flip (makeOrd (*<*))) ]
  where
  x = argTy1of2 $ argTy1of2 makeOrd

bgOrdWith2 :: (Generalizable a, Generalizable b, Generalizable c)
          => ((b -> b -> Bool) -> (c -> c -> Bool) -> a -> a -> Bool) -> [Expr]
bgOrdWith2 makeOrd = takeWhile (\_ -> hasOrd x && hasOrd y)
                   [ constant "<=" (             makeOrd (*<=*) (*<=*))
                   , constant "<"  (not .: flip (makeOrd (*<=*) (*<=*))) ]
  where
  x = argTy1of2 $ argTy1of2 makeOrd
  y = argTy1of2 . argTy1of2 $ argTy2of2 makeOrd

-- | Usage: @ins "x" (undefined :: Type)@
ins :: Generalizable a => a -> Instances
ins x = listable x +++ nameWith (name x) x +++ backgroundWith (background x) x

this :: Generalizable a
     => a -> (Instances -> Instances) -> Instances -> Instances
this x f is =
  if isListable is (typeOf x)
    then is
    else f (ins x +++ is)
-- TODO: change type to a -> [Instances -> Instances] -> Instances -> Instances

backgroundWith :: Typeable a => [Expr] -> a -> Instances
backgroundWith es x = [ Instance "Background" (typeOf x) es ]

getBackground :: Instances -> [Expr]
getBackground is = concat [es | Instance "Background" _ es <- is]

getEqInstancesFromBackground :: Instances -> Instances
getEqInstancesFromBackground is =
  [Instance "Eq" (argumentTy $ typ eq) [eq, iq] | eq <- eqs, iq <- iqs, typ eq == typ iq]
  where
  eqs = [Constant "==" e | Constant "==" e <- bg]
  iqs = [Constant "/=" e | Constant "/=" e <- bg]
  bg = getBackground is

backgroundOf :: Generalizable a => a -> [Expr]
backgroundOf x = getBackground $ instances x []

-- |  generalizes an expression by making it less defined,
--    starting with smaller changes, then bigger changes:
--
--    1: change constant to variable
--    1.1: if a variable of the constant type exists, use it
--    1.2: if not, introduce new variable
--    2: change a variable to a new variable
--
-- The above is the ideal, but let's start with a simpler algorithm:
--
--    1: change constant to hole
generalizations1 :: Instances -> Expr -> [Expr]
generalizations1 is (Var _ _)        =  []
generalizations1 is (Constant _ dx)  =
  [holeOfTy t | let t = dynTypeRep dx, isListable is t]
generalizations1 is (e1 :$ e2) =
  [holeOfTy t | isRight (etyp (e1 :$ e2))
              , let t = typ (e1 :$ e2)
              , isListable is t]
  ++ productWith (:$) (generalizations1 is e1) (generalizations1 is e2)
  ++ map (:$ e2) (generalizations1 is e1)
  ++ map (e1 :$) (generalizations1 is e2)
-- note above, I should only generalize types that I know how to enumerate,
-- i.e.: types that I have TypeInfo of!

generalizations :: Instances -> [Expr] -> [ [Expr] ]
generalizations is = map unfold . generalizations1 is . fold

productWith :: (a -> b -> c) -> [a] -> [b] -> [c]
productWith f xs ys = [f x y | x <- xs, y <- ys]

-- I don't love Option/WithOption.  It is clever but it is not __clear__.
-- Maybe remove from future versions of the tool?
data Option = MaxTests Int
            | ExtraInstances Instances
            | MaxConditionSize Int
            | MinFailures (Ratio Int)
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

-- minimum number of failures for a conditional generalization
computeMinFailures :: Testable a => a -> Int
computeMinFailures p = max 2 $ m * numerator r `div` denominator r
  where
  r = head $ [r | MinFailures r <- options p] ++ [1/20]
  m = maxTests p

computeConditionBound :: Testable a => a -> Maybe Int
computeConditionBound p = head $ [b | ConditionBound b <- options p] ++ [Just 3]

computeConstantBound :: Testable a => a -> Maybe Int
computeConstantBound p = head $ [b | ConstantBound b <- options p] ++ [Just 2]

computeDepthBound :: Testable a => a -> Maybe Int
computeDepthBound p = head $ [b | DepthBound b <- options p] ++ [Just 3]

class Testable a where
  resultiers :: a -> [[([Expr],Bool)]]
  ($-|) :: a -> [Expr] -> Bool
  tinstances :: a -> Instances
  options :: a -> Options
  options _ = []

instance Testable a => Testable (WithOption a) where
  resultiers (p `With` o) = resultiers p
  (p `With` o) $-| es     = p $-| es
  tinstances (p `With` o) = tinstances p ++ extraInstances (p `With` o)
  options    (p `With` o) = o : options p

instance Testable Bool where
  resultiers p = [[([],p)]]
  p $-| []  =  p
  p $-| _   =  error "($-|): too many arguments"
  tinstances _ = instances bool . instances int $ []

instance (Testable b, Generalizable a, Listable a) => Testable (a->b) where
  resultiers p = concatMapT resultiersFor tiers
    where resultiersFor x = mapFst (expr x:) `mapT` resultiers (p x)
          mapFst f (x,y) = (f x, y)
  p $-| []      =  error "($-|): too few arguments"
  p $-| (e:es)  =  p (eval (error "($-|): wrong type") e) $-| es
  tinstances p = instances (undefarg p) $ tinstances (p undefined)
    where
    undefarg :: (a -> b) -> a
    undefarg _ = undefined

results :: Testable a => a -> [([Expr],Bool)]
results = concat . resultiers

counterExamples :: Testable a => Int -> a -> [[Expr]]
counterExamples n p = [as | (as,False) <- take n (results p)]

counterExample :: Testable a => Int -> a -> Maybe [Expr]
counterExample n = listToMaybe . counterExamples n

counterExampleGens :: Testable a => Int -> a -> Maybe ([Expr],[[Expr]])
counterExampleGens n p = case counterExample n p of
  Nothing -> Nothing
  Just es -> Just (es,generalizationsCE n p es)

generalizationsCE :: Testable a => Int -> a -> [Expr] -> [[Expr]]
generalizationsCE n p es =
  [ canonicalizeWith is gs'
  | gs <- generalizations is es
  , gs' <- vassignments gs
  , isCounterExample n p gs'
  ]
  where
  is = tinstances p

generalizationsCEC :: Testable a => Int -> a -> [Expr] -> [(Expr,[Expr])]
generalizationsCEC n p es | maxConditionSize p <= 0 = []
generalizationsCEC n p es =
  [ (wc'', gs'')
  | gs <- generalizations is es
  , gs' <- vassignments gs
  , let thycs = theoryAndReprConds p
  , let wc = weakestCondition n thycs p gs'
  , wc /= constant "False" False
  , wc /= constant "True"  True
  , let (wc'':gs'') = canonicalizeWith is (wc:gs')
  ]
  where
  is = tinstances p

isCounterExample :: Testable a => Int -> a -> [Expr] -> Bool
isCounterExample m p = all (not . errorToFalse . (p $-|))
                     . take m
                     . grounds (tinstances p)

generalizationsCounts :: Testable a => Int -> a -> [Expr] -> [([Expr],Int)]
generalizationsCounts n p es =
  [ (canonicalizeWith is gs', countPasses n p gs')
  | gs <- generalizations is es
  , gs' <- vassignments gs
  ]
  where
  is = tinstances p

countPasses :: Testable a => Int -> a -> [Expr] -> Int
countPasses m p = length . filter (p $-|) . take m . grounds (tinstances p)

counterExampleGen :: Testable a => Int -> a -> Maybe ([Expr],Maybe [Expr])
counterExampleGen n p = case counterExampleGens n p of
  Nothing          -> Nothing
  Just (es,[])     -> Just (es,Nothing)
  Just (es,(gs:_)) -> Just (es,Just gs)

areInstancesOf :: [Expr] -> [Expr] -> Bool
es1 `areInstancesOf` es2 = length es1 == length es2
                        && and [e1 `isInstanceOf` e2 | (e1,e2) <- zip es1 es2]
-- change the above to use fold
-- maybe create a module that deals with lists of expressions, called Exprs

-- | List matches of lists of expressions if possible
--
-- > [0,1]   `matchList` [x,y]   = Just [x=0, y=1]
-- > [0,1+2] `matchList` [x,y+y] = Nothing
matchList :: [Expr] -> [Expr] -> Maybe Binds
matchList = m []
  where
  m bs [] [] = Just bs
  m bs (e1:es1) (e2:es2) =
    case matchWith bs e1 e2 of
      Nothing -> Nothing
      Just bs -> m bs es1 es2

-- list only the matches that introduce new variables (not variable-to-variable
-- matches)
newMatches :: [Expr] -> [Expr] -> Maybe Binds
e1 `newMatches` e2 = filter (not . isVar . snd) <$> e1 `matchList` e2


-- | Given tiers of atomic expressions, generates tiers of expressions combined
--   by function application.  This is done blindly so redundant expressions
--   are generated: like x + y and y + x.
expressionsTT :: [[Expr]] -> [[Expr]]
expressionsTT dss = dss \/ productMaybeWith ($$) ess ess `addWeight` 1
  where
  ess = expressionsTT dss

-- Generates expression schemas and a theory
theoryAndReprsFromPropAndAtoms :: Testable a => a -> [[Expr]] -> (Thy,[[Expr]])
theoryAndReprsFromPropAndAtoms p =
  theoryAndRepresentativesFromAtoms
    (maxConditionSize p) compareComplexity (const True) (===)
  where
  -- the following uses of keep make Speculate run faster by defaulting to
  -- "these things are not equal" even in cases that they are.  Despite
  -- failing to detect some equalities, Speculte will still be useful as a
  -- generator of quasi-canonical expressions.
  e1 === e2 = keep e1 && keep e2 && equal is m e1 e2
  keep e = maybe False (\b -> length (consts e) <= b) constBound
        && maybe False (\b ->         depthE e  <= b) depthBound
  constBound = computeConstantBound p
  depthBound = computeDepthBound p
  is = fullInstances p
  m  = maxTests p

-- tinstances including auto generated Eq instances (based on background)
fullInstances :: Testable a => a -> Instances
fullInstances p = is +++ getEqInstancesFromBackground is
  where
  is = tinstances p

theoryAndReprExprs :: Testable a => a -> (Thy,[Expr])
theoryAndReprExprs p =
    (\(thy,ess) -> (thy, concat $ take (maxConditionSize p) ess))
  . theoryAndReprsFromPropAndAtoms p
  . foldr (\/) [vs ++ esU]
  $ [ eval (error msg :: [[Expr]]) ess
    | Instance "Listable" _ [ess] <- is ]
  where
  vs = sort . map holeOfTy . filter (isListable is) . nubMergeMap (typesIn . typ) $ esU
  esU = getBackground is
  msg = "canditateConditions: wrong type, not [[Expr]]"
  is = tinstances p

theoryAndReprConds :: Testable a => a -> (Thy, [Expr])
theoryAndReprConds p = (thy, expr True : filter (\c -> typ c == boolTy && hasVar c) es)
  where
  (thy,es) = theoryAndReprExprs p

weakestCondition :: Testable a => Int -> (Thy,[Expr]) -> a -> [Expr] -> Expr
weakestCondition m (thy,cs) p es = fst
                                 . maximumOn snd
                                 . takeBound (computeConditionBound p) $
  [ (c,n) | c <- map fst $ classesFromSchemasAndVariables thy (uncurry (flip Var) <$> vars es) cs
          , not (isAssignment c)
          , not (isAssignmentTest is (maxTests p) c)
          , let (is,n) = isCounterExampleUnder m p c es
          , is
          ] ++ [(expr False,0)]
  where
  is = tinstances p

isCounterExampleUnder :: Testable a => Int -> a -> Expr -> [Expr] -> (Bool, Int)
isCounterExampleUnder m p c es = and'
  [ not . errorToFalse $ p $-| es'
  | (bs,es') <- take m $ groundsAndBinds (tinstances p) es
  , errorToFalse $ eval False (c `assigning` bs)
  ]
  where
  and' ps = (and ps && length ps > computeMinFailures p, length ps)

isVar :: Expr -> Bool
isVar (Var _ _) = True
isVar _         = False

fromBackgroundOf :: (Generalizable a, Typeable b) => String -> a -> Maybe b
fromBackgroundOf nm = listToMaybe
                    . catMaybes
                    . map evaluate
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
