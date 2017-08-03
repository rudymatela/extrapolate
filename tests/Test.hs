-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
module Test
  ( module Test.Extrapolate

  , reportTests
  , getMaxTestsFromArgs
  , mainTest
  , printLines

  , (-:-), ll, llb

  , zero, one

  , false, true

  , nothing, nothingBool, just

  , comma
  )
where

import System.Exit (exitFailure)
import Data.List (elemIndices)
import System.Environment (getArgs)
import Test.Speculate.Expr (typ)

import Test.Extrapolate

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
