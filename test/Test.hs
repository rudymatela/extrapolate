-- Copyright (c) 2017-2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
module Test
  ( module Test.Extrapolate
  , module Test.Extrapolate.Core
  , module Test.LeanCheck.Utils.Operators

  , reportTests
  , getMaxTestsFromArgs
  , mainTest
  , printLines

  , operatorE

  -- * Properties and Tests
  , generalizableOK
  , exprOK
  , idExprEval, showOK
  , instancesOK
  , namesOK, sameNamesIn, namesIn
  , tiersOK, sameTiersIn, tiersIn
  , bgEqOK, bgEqOrdOK

  , subset, bgSubset

  , instancesSubset

  , Thy

  , module Data.Haexpress.Fixtures
  )
where

import System.Exit (exitFailure)
import Data.List (elemIndices)
import System.Environment (getArgs)
import Data.Typeable (typeOf)
import Data.List (isPrefixOf, sort)
import Test.Speculate.Reason

import Test.Extrapolate
import Test.Extrapolate.Utils
import Test.Extrapolate.Core
import Data.Haexpress.Fixtures hiding (nubVars, canonicalizeWith, canonicalVariations)
import Test.LeanCheck.Utils.Operators

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n = do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n' = do
  n <- getMaxTestsFromArgs n'
  reportTests (tests n)

printLines :: Show a => [a] -> IO ()
printLines = putStrLn . unlines . map show

operatorE :: Expr -> Expr
operatorE ((opE :$ _) :$ _)  =  opE
operatorE _  =  error "operatorE: not a binary operator"



-- Properties and tests --

generalizableOK :: (Eq a, Show a, Generalizable a) => Int -> a -> Bool
generalizableOK n x = holds n (exprOK -:> x)
                   && instancesOK (und -: x)
  where
  und = error "generalizableOK: this should not get evaluated"

exprOK :: (Eq a, Show a, Generalizable a) => a -> Bool
exprOK = idExprEval &&& showOK

idExprEval :: (Eq a, Generalizable a) => a -> Bool
idExprEval x = eval (error "idExprEval: could not eval") (expr x) == x

showOK :: (Show a, Generalizable a) => a -> Bool
showOK x = show x == dropType (show (expr x))
  where
  dropType :: String -> String
  dropType cs     | " :: " `isPrefixOf` cs = ""
  dropType ""     =  ""
  dropType (c:cs) =  c : dropType cs

instancesOK :: (Eq a, Generalizable a) => a -> Bool
instancesOK = namesOK &&& tiersOK

namesOK :: Generalizable a => a -> Bool
namesOK x =  name x == head (x `namesIn` x)
          && x `sameNamesIn` [x]
          && x `sameNamesIn` [[x]]
          && x `sameNamesIn` mayb x
          && x `sameNamesIn` (x,x)
          && x `sameNamesIn` (x,())
          && x `sameNamesIn` ((),x)
          && x `sameNamesIn` (x,(),())
          && x `sameNamesIn` ((),x,())
          && x `sameNamesIn` ((),(),x)

sameNamesIn :: (Generalizable a, Generalizable b) => a -> b -> Bool
x `sameNamesIn` c = x `namesIn` x
           =| 12 |= x `namesIn` c

namesIn :: (Generalizable a, Generalizable b) => a -> b -> [String]
x `namesIn` c = lookupNames (instances c []) (val x)

tiersOK :: (Eq a, Generalizable a) => a -> Bool
tiersOK x =  x `sameTiersIn` x
          && x `sameTiersIn` [x]
          && x `sameTiersIn` [[x]]
          && x `sameTiersIn` (mayb x)
          && x `sameTiersIn` (x,x)
          && x `sameTiersIn` (x,())
          && x `sameTiersIn` ((),x)
          && x `sameTiersIn` (x,(),())
          && x `sameTiersIn` ((),x,())
          && x `sameTiersIn` ((),(),x)

bgEqOK :: (Eq a, Generalizable a) => a -> a -> Bool
bgEqOK = (*==*) ==== (==)
    &&&& (*/=*) ==== (/=)

bgEqOrdOK :: (Eq a, Ord a, Generalizable a) => a -> a -> Bool
bgEqOrdOK = bgEqOK &&&& (*<=*) ==== (<=)
                   &&&& (*<*)  ==== (<)

sameTiersIn :: (Eq a, Generalizable a, Generalizable b) => a -> b -> Bool
x `sameTiersIn` cx = isListableT (instances cx []) (typeOf x)
                  && (tiers -: [[x]]) =| 6 |= tiersIn cx

tiersIn :: (Generalizable a, Generalizable b) => b -> [[a]]
tiersIn c = ret
  where
  ret = mapT (eval . error $ "tiersIn: the imposible happened")
      $ tiersE (instances c []) (typeOf (head (head ret)))

subset :: Ord a => [a] -> [a] -> Bool
xs `subset` ys = sort xs `isSubsequenceOf` sort ys

bgSubset :: (Generalizable a, Generalizable b) => a -> b -> Bool
x `bgSubset` y = backgroundOf x `subset` backgroundOf y

instancesSubset :: (Eq a, Eq b, Generalizable a, Generalizable b) => a -> b -> Bool
x `instancesSubset` y = x `bgSubset` y
                     && x `sameTiersIn` y
                     && x `sameNamesIn` y

-- available on Data.List since GHC >= 8.0
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _  = True
isSubsequenceOf (_:_) [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y    =    xs  `isSubsequenceOf` ys
  | otherwise = (x:xs) `isSubsequenceOf` ys

-- Quick and Dirty!
instance Show Thy where
  show Thy { rules = rs
           , equations = eqs
           , canReduceTo = (->-)
           , closureLimit = cl
           , keepE = keep
           }
    = "Thy { rules = "
   ++ drop 14 (indent 14 . listLines $ map showEquation rs)
   ++ "    , equations = "
   ++ drop 18 (indent 18 . listLines $ map showEquation eqs)
   ++ "    , canReduceTo = " ++ showCanReduceTo (->-) ++ "\n"
   ++ "    , closureLimit = " ++ show cl ++ "\n"
   ++ "    , keepE = " ++ showKeepE keep ++ "\n"
   ++ "    }"
    where
    showEquation (e1,e2) = showExpr e1 ++ " == " ++ showExpr e2
    listLines [] = "[]"
    listLines ss = '[':(tail . unlines $ map (", " ++) ss) ++ "]"
    showCanReduceTo (->-) = "(??)"
    showKeepE keep = "\\e -> ??"
    indent :: Int -> String -> String
    indent n = unlines . map (replicate n ' ' ++) . lines


