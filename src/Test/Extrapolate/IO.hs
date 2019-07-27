-- |
-- Module      : Test.Extrapolate.IO
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- QuickCheck-like interface.
{-# LANGUAGE CPP #-}
module Test.Extrapolate.IO
  ( check
  , checkResult

  , for
  , withInstances
  , withBackground
  , withConditionSize
  )
where

#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#endif

import Test.Extrapolate.Core
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (find)
import Control.Monad
import Control.Exception as E (SomeException, catch, evaluate)

-- | Use @`for`@ to configure the number of tests performed by @check@.
--
-- > > check `for` 10080 $ \xs -> sort (sort xs) == sort (xs :: [Int])
-- > +++ OK, passed 10080 tests.
--
-- Don't forget the dollar ('$')!
for :: Testable a => (WithOption a -> b) -> Int -> a -> b
check `for` m  =  \p -> check $ p `With` MaxTests m

-- | Allows the user to customize instance information available when generalized.
--   (For advanced users.)
withInstances :: Testable a => (WithOption a -> b) -> Instances -> a -> b
check `withInstances` is  =  \p -> check $ p `With` ExtraInstances is

-- | Use @`withBackground`@ to provide additional functions to appear in side-conditions.
--
-- > check `withBackground` [value "isSpace" isSpace] $ \xs -> unwords (words xs) == xs
-- > *** Failed! Falsifiable (after 4 tests):
-- > " "
-- >
-- > Generalization:
-- > ' ':_
-- >
-- > Conditional Generalization:
-- > c:_  when  isSpace c
withBackground :: Testable a => (WithOption a -> b) -> [Expr] -> a -> b
check `withBackground` ufs  =  check `withInstances` [value "background" ufs]

-- | Use @`withConditionSize`@ to configure the maximum condition size allowed.
withConditionSize :: Testable a => (WithOption a -> b) -> Int -> a -> b
check `withConditionSize` s  =   \p -> check $ p `With` MaxConditionSize s

-- | Checks a property printing results on 'stdout'
--
-- > > check $ \xs -> sort (sort xs) == sort (xs::[Int])
-- > +++ OK, passed 360 tests.
-- >
-- > > check $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
-- > *** Failed! Falsifiable (after 4 tests):
-- > [] [0,0]
-- >
-- > Generalization:
-- > [] (x:x:_)
check :: Testable a => a -> IO ()
check = void . checkResult

-- | Check a property
--   printing results on 'stdout' and
--   returning 'True' on success.
--
-- There is no option to silence this function:
-- for silence, you should use 'Test.LeanCheck.holds'.
checkResult :: Testable a => a -> IO Bool
checkResult p = do
  (r,ces) <- resultIO p
  putStr . showResult p ces $ r
  return (isOK r)
  where
  isOK (OK _) = True
  isOK _      = False

data Result = OK        Int
            | Falsified Int Expr
            | Exception Int Expr String
  deriving (Eq, Show)

resultsIO :: Testable a => a -> IO [Result]
resultsIO = zipWithM torio [1..] . limitedResults
  where
    tor i (_,True) = OK i
    tor i (as,False) = Falsified i as
    torio i r@(as,_) = E.evaluate (tor i r)
       `catch` \e -> let _ = e :: SomeException
                     in return (Exception i as (show e))

resultIO :: Testable a => a -> IO (Result, [Expr])
resultIO p = do
  rs <- resultsIO p
  return ( fromMaybe (last rs) $ find isFailure rs
         , mapMaybe ce rs )
  where
  isFailure (OK _) = False
  isFailure _      = True
  ce (OK _)             = Nothing
  ce (Falsified _ es)   = Just es
  ce (Exception _ es _) = Just es

showResult :: Testable a => a -> [Expr] -> Result -> String
showResult p ces (OK n)             = "+++ OK, passed " ++ show n ++ " tests"
                                   ++ takeWhile (\_ -> n < maxTests p) " (exhausted)" ++ ".\n\n"
showResult p ces (Falsified i ce)   = "*** Failed! Falsifiable (after "
                                   ++ show i ++ " tests):\n" ++ showCEandGens p ce
showResult p ces (Exception i ce e) = "*** Failed! Exception '" ++ e ++ "' (after "
                                   ++ show i ++ " tests):\n" ++ showCEandGens p ce

showCEandGens :: Testable a => a -> Expr -> String
showCEandGens p e = showCE e ++ "\n\n"
  ++ case gens e of
       []    -> ""
       (e:_) -> "Generalization:\n"
             ++ showCE (canonicalizeWith (namesFor p) e) ++ "\n\n"
  ++ case cgens e of
       []         -> ""
       (e:_) -> "Conditional Generalization:\n"
              ++ showCCE (canonicalizeWith (namesFor p) e) ++ "\n\n"
  where
  gens = counterExampleGeneralizations (groundsFor p)
  cgens = conditionalCounterExampleGeneralizations
    (maxConditionSize p)
    (atomsFor p)
    (groundsFor p)
    (mkEquationFor p)

showCE :: Expr -> String
showCE = s . tail . unfoldApp
  where
  s [e] = showPrecExpr 0 e
  s es = unwords [showPrecExpr 11 e | e <- es]


showCCE :: Expr -> String
showCCE  =  s . unfoldApp
  where
  s [Value "==>" _,c,e]  =  showCE e ++ "  when  " ++ showPrecExpr 0 (prettify c)
  s _  =  error "showCCE: not a conditional counterexample"

-- WARNING: expressions are unevaluable after this, just good for printing
prettify :: Expr -> Expr
prettify (((Value "<=" _) :$ e1) :$ e2) | size e1 < size e2 = (((Value ">=" undefined) :$ e2) :$ e1)
prettify (((Value "<"  _) :$ e1) :$ e2) | size e1 < size e2 = (((Value ">"  undefined) :$ e2) :$ e1)
prettify (e1 :$ e2) = prettify e1 :$ prettify e2
prettify e = e
