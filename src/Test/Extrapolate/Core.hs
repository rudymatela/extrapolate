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

  , Option (..)
  , options
  , WithOption (..)
  , maxTests
  , extraInstances
  , maxConditionSize
  , hasEq
  , (*==*)

  , counterExampleGen
  , counterExampleGens

  , generalizations
  , generalizationsCE
  , generalizationsCEC
  , generalizationsCounts

  , weakestCondition
  , candidateConditions

  , conditionalGeneralization
  , matchList
  , newMatches

  , Testable
  , results

  , areInstancesOf

  , expressionsT
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
import Data.List (insert)
import Data.Functor ((<$>)) -- for GHC <= 7.8
import Test.Extrapolate.Exprs
import Test.LeanCheck.Error (errorToFalse)
import Test.Extrapolate.TypeBinding -- for Haddock

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
  background _ = [ constant "not" not ]
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
  background mx  =  [ constant "Just"    (Just   ->: mx) ]
  instances mx  =  this mx $ instances (fromJust mx)

instance (Generalizable a, Generalizable b) => Generalizable (Either a b) where
  expr lx@(Left x)   =  constant "Left"  (Left  ->: lx) :$ expr x
  expr ry@(Right y)  =  constant "Right" (Right ->: ry) :$ expr y
  name exy = "e" ++ name (fromLeft exy) ++ name (fromRight exy)
  background exy  =  [ constant "Left"  (Left  ->: exy)
                     , constant "Right" (Right ->: exy) ]
  instances exy  =  this exy $ instances (fromLeft  exy)
                             . instances (fromRight exy)

instance (Generalizable a, Generalizable b) => Generalizable (a,b) where
  name xy  =  name (fst xy) ++ name (snd xy)
  expr (x,y)  =  constant "," ((,) ->>: (x,y))
              :$ expr x :$ expr y
  instances xy  =  this xy $ instances (fst xy)
                           . instances (snd xy)

instance (Generalizable a, Generalizable b, Generalizable c)
      => Generalizable (a,b,c) where
  name xyz  =  name ((\(x,_,_) -> x) xyz)
            ++ name ((\(_,y,_) -> y) xyz)
            ++ name ((\(_,_,z) -> z) xyz)
  expr (x,y,z)  =  constant ",," ((,,) ->>>: (x,y,z))
                :$ expr x :$ expr y :$ expr z
  instances xyz  =  this xyz $ instances (fst xyz)
                             . instances (snd xyz)
                             . instances (trd xyz)
    where
    fst (x,_,_) = x
    snd (_,y,_) = y
    trd (_,_,z) = z

instance (Generalizable a, Generalizable b, Generalizable c, Generalizable d)
      => Generalizable (a,b,c,d) where
  name xyzw  =  name ((\(x,_,_,_) -> x) xyzw)
             ++ name ((\(_,y,_,_) -> y) xyzw)
             ++ name ((\(_,_,z,_) -> z) xyzw)
             ++ name ((\(_,_,_,w) -> w) xyzw)
  expr (x,y,z,w)  =  constant ",,," ((,,,) ->>>>: (x,y,z,w))
                  :$ expr x :$ expr y :$ expr z :$ expr w
  instances xyzw  =  this xyzw $ instances (fst xyzw)
                               . instances (snd xyzw)
                               . instances (trd xyzw)
                               . instances (fth xyzw)
    where
    fst (x,_,_,_) = x
    snd (_,y,_,_) = y
    trd (_,_,z,_) = z
    fth (_,_,_,w) = w

instance Generalizable a => Generalizable [a] where
  name xs  =  name (head xs) ++ "s"
  expr (xs@[])      =  showConstant  ([]    -: xs)
  expr (xs@(y:ys))  =  constant ":"  ((:) ->>: xs) :$ expr y :$ expr ys
  background xs  =  [ constant "length" (length -:> xs)
                    , constant "any"    (any    ->:> xs)
                    , constant "all"    (all    ->:> xs)
                    , constant "filter" (filter ->:> xs) ]
                 ++ [ constant "elem" (elemBy (*==*) ->:> xs) | hasEq (head xs) ]
  instances xs  =  this xs $ instances (head xs)
-- TODO: add (==) and (/=) when list element type has (==) and (/=)
-- TODO: add (<=) and (<)  when list element type has (<=) and (<)

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
  deriving (Show, Typeable) -- Typeable needed for GHC <= 7.8

data WithOption a = With
                  { property :: a
                  , option :: Option }

type Options = [Option]

maxTests :: Testable a => a -> Int
maxTests p = head $ [m | MaxTests m <- options p] ++ [360]

extraInstances :: Testable a => a -> Instances
extraInstances p = concat [is | ExtraInstances is <- options p]

maxConditionSize :: Testable a => a -> Int
maxConditionSize p = head $ [m | MaxConditionSize m <- options p] ++ [5]

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
  tinstances _ = []

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
generalizationsCEC n p es =
  [ (wc'', gs'')
  | gs <- generalizations is es
  , gs' <- vassignments gs
  , let wc = weakestCondition n p gs'
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


expressionsT :: [Expr] -> [[Expr]]
expressionsT ds = [ds] \/ productMaybeWith ($$) es es `addWeight` 1
  where
  es = expressionsT ds

expressionsTT :: [[Expr]] -> [[Expr]]
expressionsTT dss = dss \/ productMaybeWith ($$) ess ess `addWeight` 1
  where
  ess = expressionsTT dss

-- given a 100% generalization, >90% generalization, returns a conditional generalization
conditionalGeneralization :: Testable a => Int -> a -> [Expr] -> [Expr] -> Maybe ([Expr],[Expr])
conditionalGeneralization m p es0 es1 = listToMaybe
  [ ([c],es1)
  | isJust $ es0 `newMatches` es1
  , c <- candidates
  , typ c == boolTy
  , any (`elem` vars [c]) [(t,x) | Var x t <- vs]
  , isCounterExampleUnder m p c es1
  ]
  where
  Just esM = es0 `newMatches` es1
  candidates = concat . take (maxConditionSize p) . expressionsT $ vs ++ esU
  vs = reverse [Var x (typ e) | (x,e) <- esM]
  esU = concat [es | Instance "Background" _ es <- tinstances p]

weakestCondition :: Testable a => Int -> a -> [Expr] -> Expr
weakestCondition m p es = head $
  [ c | c <- candidateConditions p es
      , isCounterExampleUnder m p c es
      ] ++ [expr False]
  where

candidateConditions :: Testable a => a -> [Expr] -> [Expr]
candidateConditions p es = expr True :
  [ c
  | c <- candidateExpressions p es
  , typ c == boolTy
  , not (isAssignment c)
  , not (isAssignmentTest is (maxTests p) c)
  ]
  where
  is = tinstances p

-- | Canditate expressions to appear in conditions
candidateExpressions :: Testable a => a -> [Expr] -> [Expr]
candidateExpressions p es = concat . take (maxConditionSize p) . expressionsTT
                         . foldr (\/) [vs ++ esU]
                         $ [ eval (error msg :: [[Expr]]) ess
                           | Instance "Listable" _ [ess] <- is ]
  where
  vs = [Var n t | (t,n) <- vars es]
  esU = concat [es | Instance "Background" _ es <- is]
  msg = "canditateConditions: wrong type, not [[Expr]]"
  is = tinstances p

isCounterExampleUnder :: Testable a => Int -> a -> Expr -> [Expr] -> Bool
isCounterExampleUnder m p c es = and'
  [ not . errorToFalse $ p $-| es'
  | (bs,es') <- take m $ groundsAndBinds (tinstances p) es
  , errorToFalse $ eval False (c `assigning` bs)
  ]
  where
  and' ps = and ps && length ps > m `div` 12 -- poor workaround

isVar :: Expr -> Bool
isVar (Var _ _) = True
isVar _         = False

hasEq :: Generalizable a => a -> Bool
hasEq x = isJust $ "==" `fromBackgroundOf` x -: mayb (x >- x >- bool)

(*==*) :: Generalizable a => a -> a -> Bool
x *==* y = x == y
  where
  (==) = fromMaybe (error "(*==*): no (==) operator in background")
       $ "==" `fromBackgroundOf` x
-- TODO: rename (*==*) to (-==-), use it in the Test module.

fromBackgroundOf :: (Generalizable a, Typeable b) => String -> a -> Maybe b
fromBackgroundOf nm = listToMaybe
                    . catMaybes
                    . map evaluate
                    . filter (`isConstantNamed` nm)
                    . background
