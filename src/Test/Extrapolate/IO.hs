-- |
-- Module      : Test.Extrapolate.IO
-- Copyright   : (c) 2017 Rudy Matela
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
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (find, intercalate)
import Control.Exception as E (SomeException, catch, evaluate)

-- | Use @`for`@ to configure the number of tests performed by @check@.
--
-- > > check `for` 10080 $ \xs -> sort (sort xs) == sort (xs :: [Int])
-- > +++ OK, passed 10080 tests.
--
-- Don't forget the dollar (@$@)!
for :: Testable a => (WithOption a -> b) -> Int -> a -> b
check `for` m  =  \p -> check $ p `With` MaxTests m

-- | Allows the user to customize instance information available when generalized.
--   (For advanced users.)
withInstances :: Testable a => (WithOption a -> b) -> Instances -> a -> b
check `withInstances` is  =  \p -> check $ p `With` ExtraInstances is

-- | Use @`withBackground`@ to provide additional functions to appear in side-conditions.
--
-- > check `withBackground` [constant "isSpace" isSpace] $ \xs -> unwords (words xs) == xs
-- > *** Failed! Falsifiable (after 4 tests):
-- > " "
-- >
-- > Generalization:
-- > ' ':_
-- >
-- > Conditional Generalization:
-- > c:_  when  isSpace c
withBackground :: Testable a => (WithOption a -> b) -> [Expr] -> a -> b
check `withBackground` ufs  =  check `withInstances` backgroundWith ufs (undefined::Option)

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
check p = checkResult p >> return ()

-- | Check a property
--   printing results on 'stdout' and
--   returning 'True' on success.
--
-- There is no option to silence this function:
-- for silence, you should use 'Test.LeanCheck.holds'.
checkResult :: Testable a => a -> IO Bool
checkResult p = do
  (r,ces) <- resultIO m p
  putStr . showResult m p ces $ r
  return (isOK r)
  where
  m = maxTests p
  isOK (OK _) = True
  isOK _      = False

data Result = OK        Int
            | Falsified Int [Expr]
            | Exception Int [Expr] String
  deriving (Eq, Show)

resultsIO :: Testable a => Int -> a -> IO [Result]
resultsIO n = sequence . zipWith torio [1..] . take n . results
  where
    tor i (_,True) = OK i
    tor i (as,False) = Falsified i as
    torio i r@(as,_) = E.evaluate (tor i r)
       `catch` \e -> let _ = e :: SomeException
                     in return (Exception i as (show e))

resultIO :: Testable a => Int -> a -> IO (Result, [[Expr]])
resultIO n p = do
  rs <- resultsIO n p
  return ( maybe (last rs) id $ find isFailure rs
         , mapMaybe ce rs )
  where
  isFailure (OK _) = False
  isFailure _      = True
  ce (OK _)             = Nothing
  ce (Falsified _ es)   = Just es
  ce (Exception _ es _) = Just es

showResult :: Testable a => Int -> a -> [[Expr]] -> Result -> String
showResult m p ces (OK n)             = "+++ OK, passed " ++ show n ++ " tests"
                                     ++ takeWhile (\_ -> n < m) " (exhausted)" ++ ".\n\n"
showResult m p ces (Falsified i ce)   = "*** Failed! Falsifiable (after "
                                     ++ show i ++ " tests):\n" ++ showCEC m p ce
showResult m p ces (Exception i ce e) = "*** Failed! Exception '" ++ e ++ "' (after "
                                     ++ show i ++ " tests):\n" ++ showCEC m p ce

showCEC :: Testable a => Int -> a -> [Expr] -> String
showCEC m p es = showCE es ++ "\n\n"
  ++ case generalizationsCE m p es of
       []     -> ""
       (es:_) -> "Generalization:\n"
              ++ showCE es ++ "\n\n"
  ++ case generalizationsCEC m p es of
       []         -> ""
       (es:_) -> "Conditional Generalization:\n"
              ++ showCCE es ++ "\n\n"

showCE :: [Expr] -> String
showCE [e] = showPrecExpr 0 e
showCE es = unwords [showPrecExpr 11 e | e <- es]

showCCE :: (Expr,[Expr]) -> String
showCCE (c,es) = showCE es ++ "  when  " ++ showPrecExpr 0 (prettify c)

-- WARNING: expressions are unevaluable after this, just good for printing
prettify :: Expr -> Expr
prettify (((Constant "<=" _) :$ e1) :$ e2) | lengthE e1 < lengthE e2 = (((Constant ">=" undefined) :$ e2) :$ e1)
prettify (((Constant "<"  _) :$ e1) :$ e2) | lengthE e1 < lengthE e2 = (((Constant ">"  undefined) :$ e2) :$ e1)
prettify (e1 :$ e2) = prettify e1 :$ prettify e2
prettify e = e
