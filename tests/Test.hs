-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
module Test
  ( module Test.Extrapolate
  , module Test.Extrapolate.Core
  , module Test.LeanCheck.Utils.Operators

  , reportTests
  , getMaxTestsFromArgs
  , mainTest
  , printLines

  , (-:-), ll, llb

  , zero, one

  , false, true

  , nothing, nothingBool, just

  , comma

  -- * Properties and Tests
  , generalizableOK
  , idExprEval, showOK
  , instancesOK
  , namesOK, sameNamesIn, namesIn
  , tiersOK, sameTiersIn, tiersIn

  , subset, bgSubset

  , instancesSubset
  )
where

import System.Exit (exitFailure)
import Data.List (elemIndices)
import System.Environment (getArgs)
import Test.Speculate.Expr (typ)
import Test.Speculate.Expr.Instance as I
import Data.Typeable (typeOf)
import Data.List (isPrefixOf, sort)

import Test.Extrapolate
import Test.Extrapolate.Core hiding (false, true)
import qualified Test.Extrapolate.Core as Core
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

(-:-) :: Expr -> Expr -> Expr
x -:- xs  =  consE :$ x :$ xs
  where
  consE = case show $ typ x of
            "Int"  -> consEint
            "Bool" -> consEbool
            t      -> error $ "(-:-): unhandled type " ++ t
  consEint   =  constant ":" ((:) -:> int)
  consEbool  =  constant ":" ((:) -:> bool)
infixr 5 -:-

ll :: Expr
ll  =  expr ([] :: [Int])

zero :: Expr
zero  =  expr (0 :: Int)

one :: Expr
one  =  expr (1 :: Int)

llb :: Expr
llb  =  expr ([] :: [Bool])

false :: Expr
false  =  expr False

true :: Expr
true  =  expr True

nothing :: Expr
nothing  =  constant "Nothing" (Nothing :: Maybe Int)

nothingBool :: Expr
nothingBool  =  constant "Nothing" (Nothing :: Maybe Bool)

just :: Expr -> Expr
just x  =  justE :$ x
  where
  justE = case show $ typ x of
            "Int"  -> justEint
            "Bool" -> justEbool
            t      -> error $ "(-:-): unhandled type " ++ t
  justEint   =  constant "Just" (Just -:> int)
  justEbool  =  constant "Just" (Just -:> bool)

comma :: Expr -> Expr -> Expr
comma x y  =  commaE :$ x :$ y
  where
  commaE  =  case (show $ typ x, show $ typ y) of
               ("Int", "Int")  -> commaEii
               ("Int", "Bool") -> commaEib
               ("Bool","Int")  -> commaEbi
               ("Bool","Bool") -> commaEbb
               (t,t')          -> error $ "(-:-): unhandled types " ++ t ++ " " ++ t'
  commaEii  =  constant "," ((,) ->>: (int,int))
  commaEib  =  constant "," ((,) ->>: (int,bool))
  commaEbi  =  constant "," ((,) ->>: (bool,int))
  commaEbb  =  constant "," ((,) ->>: (bool,bool))


-- Properties and tests --

generalizableOK :: (Eq a, Show a, Generalizable a) => Int -> a -> Bool
generalizableOK n x = holds n (exprOK -:> x)
                   && instancesOK x

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
namesOK x =  Core.name x == head (x `namesIn` x)
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
x `namesIn` c = I.names (instances c []) (typeOf x)

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

sameTiersIn :: (Eq a, Generalizable a, Generalizable b) => a -> b -> Bool
x `sameTiersIn` cx = isListable (instances cx []) (typeOf x)
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
