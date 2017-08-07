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
  , usefuns
  , (+++)
  , nameOf
  , backgroundOf

  , Option (..)
  , options
  , WithOption (..)
  , maxTests
  , extraInstances
  , maxConditionSize

  , counterExampleGen
  , counterExampleGens

  , generalizationsCE
  , generalizationsCEC
  , generalizationsCounts

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
import Data.Maybe (listToMaybe, fromJust, isJust)
import Data.Either (isRight)
import Data.List (insert)
import Data.Functor ((<$>)) -- for GHC <= 7.8
import Test.Extrapolate.Exprs
import Test.LeanCheck.Error (errorToFalse)

class (Listable a, Typeable a, Show a) => Generalizable a where
  expr :: a -> Expr
  name :: a -> String
  name _ = "x"
  background :: a -> [Expr]
  background _ = []
  instances :: a -> Instances -> Instances
-- TODO: change Generalizable to include name:
--   name :: a -> String

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
  background x = [ constant "==" ((==) -:> x)
                 , constant "/=" ((/=) -:> x)
                 , constant "<"  ((<)  -:> x)
                 , constant "<=" ((<=) -:> x)
                 ]
  instances x = this x id

instance Generalizable Integer where
  expr = showConstant
  name _ = "x"
  background x = [ constant "==" ((==) -:> x)
                 , constant "/=" ((/=) -:> x)
                 , constant "<"  ((<)  -:> x)
                 , constant "<=" ((<=) -:> x) ]
  instances x = this x id

instance Generalizable Char where
  expr = showConstant
  name _ = "c"
  background c = [ constant "==" ((==) -:> c)
                 , constant "/=" ((/=) -:> c)
                 , constant "<"  ((<)  -:> c)
                 , constant "<=" ((<=) -:> c) ]
  instances c = this c id

instance (Generalizable a) => Generalizable (Maybe a) where
  expr mx@Nothing   =  constant "Nothing" (Nothing -: mx)
  expr mx@(Just x)  =  constant "Just"    (Just   ->: mx) :$ expr x
  name _ = "mx" -- TODO: use name of inner type
  background mx  =  [ constant "Just"    (Just   ->: mx) ]
  instances mx  =  this mx $ instances (fromJust mx)

instance (Generalizable a, Generalizable b) => Generalizable (a,b) where
  name _  =  "xy" -- TODO: use names of inner types
  expr (x,y)  =  constant "," ((,) ->>: (x,y))
              :$ expr x :$ expr y
  instances xy  =  this xy $ instances (fst xy)
                           . instances (snd xy)

instance (Generalizable a, Generalizable b, Generalizable c) => Generalizable (a,b,c) where
  name _  =  "xyz" -- TODO: use names of inner types
  expr (x,y,z)  =  constant ",," ((,,) ->>>: (x,y,z))
                :$ expr x :$ expr y :$ expr z
  instances xyz  =  this xyz $ instances (fst xyz)
                             . instances (snd xyz)
                             . instances (trd xyz)
    where
    fst (x,_,_) = x
    snd (_,y,_) = y
    trd (_,_,z) = z

instance Generalizable a => Generalizable [a] where
  name xs  =  name (head xs) ++ "s"
  expr (xs@[])      =  showConstant  ([]    -: xs)
  expr (xs@(y:ys))  =  constant ":"  ((:) ->>: xs) :$ expr y :$ expr ys
  background xs  =  [ constant "length" (length -:> xs)
                   , constant "filter" (filter ->:> xs) ]
  instances xs  =  this xs $ instances (head xs)

nameOf :: Generalizable a => a -> String
nameOf x = head $ names (instances x []) (typeOf x)

-- | Usage: @ins "x" (undefined :: Type)@
ins :: Generalizable a => a -> Instances
ins x = listable x +++ nameWith (name x) x +++ usefuns x (background x)

this :: Generalizable a
     => a -> (Instances -> Instances) -> Instances -> Instances
this x f is =
  if isListable is (typeOf x)
    then is
    else f (ins x +++ is)

-- bad function naming!
-- TODO: rename to makeBackground
usefuns :: Typeable a => a -> [Expr] -> Instances
usefuns x es = [ Instance "Background" (typeOf x) es ]

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
  [ c
  | c <- constant "True" True : candidates
  , typ c == boolTy
  , not (isAssignment c)
  , not (isAssignmentTest is m c)
  , isCounterExampleUnder m p c es
  ] ++ [ constant "False" False ]
  where
  is = tinstances p
  candidates = concat . take (maxConditionSize p) . expressionsTT
             . foldr (\/) [vs ++ esU]
             $ [ eval (error msg :: [[Expr]]) ess
               | Instance "Listable" _ [ess] <- tinstances p ]
  vs = [Var n t | (t,n) <- vars es]
  esU = concat [es | Instance "Background" _ es <- tinstances p]
  msg = "weakestCondition: wrong type, not [[Expr]]"

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
