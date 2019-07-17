{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
-- Copyright (c) 2017-2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.List (sort, nub)

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable NOrd
#endif

data NOrd = NOrd
  deriving Show

deriveListable ''NOrd
deriveGeneralizable ''NOrd

nord :: NOrd
nord = undefined

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  -- Transforming lists into Exprs
  , expr ([]::[Int]) == value "[]" ([]::[Int])
  , expr ([0::Int])  == zero -:- ll
  , expr ([0::Int,1])  == zero -:- one -:- ll
  , holds n $ \xs -> expr xs == foldr (-:-) ll (map expr (xs :: [Int]))
  , holds n $ \ps -> expr ps == foldr (-:-) llb (map expr (ps :: [Bool]))

  -- Transforming Maybes into Exprs
  , expr (Nothing    :: Maybe Int)   ==  nothing
  , expr (Nothing    :: Maybe Bool)  ==  nothingBool
  , expr (Just 1     :: Maybe Int)   ==  just one
  , expr (Just False :: Maybe Bool)  ==  just false
  , holds n $ \x -> expr (Just x) == just (expr (x :: Int))
  , holds n $ \p -> expr (Just p) == just (expr (p :: Bool))

  -- Transforming Tuples into Exprs
  , expr ((0,False) :: (Int,Bool))  ==  pair zero false
  , expr ((True,1)  :: (Bool,Int))  ==  pair true one

  -- Showing of Exprs
  , holds n $ \x -> show (expr x) == show (x :: ()) ++ " :: ()"
  , holds n $ \x -> show (expr x) == show (x :: Int) ++ " :: Int"
  , holds n $ \p -> show (expr p) == show (p :: Bool) ++ " :: Bool"
  , holds n $ \x -> show (expr x) == show (x :: ((),Maybe Int,[Bool]))
                                        ++ " :: ((),(Maybe Int),[Bool])"

  -- Generalizable instance properties
  , generalizableOK n ()
  , generalizableOK n int
  , generalizableOK n integer
  , generalizableOK n bool
  , generalizableOK n char
  , generalizableOK n ordering
  , generalizableOK 9 [()]
  , generalizableOK n [int]
  , generalizableOK n [integer]
  , generalizableOK n [bool]
  , generalizableOK n [char]
  , generalizableOK n [ordering]
  , generalizableOK n (mayb ())
  , generalizableOK n (mayb int)
  , generalizableOK n (mayb integer)
  , generalizableOK n (mayb bool)
  , generalizableOK n (mayb char)
  , generalizableOK n (eith () int)
  , generalizableOK n (eith integer bool)
  , generalizableOK n (int,bool)
  , generalizableOK n ((),integer)
  , generalizableOK n ((),bool,integer)
  , generalizableOK n (int,(),bool,integer)
  , generalizableOK n (int,(),bool,integer,char)
  , generalizableOK n (string,int,(),bool,integer,char)
-- TODO: implement further tuple instances (7,8,9,10,11,12) and uncomment below
  , generalizableOK n ((),(),(),(),(),(),())
#if __GLASGOW_HASKELL__ < 710
-- no 8-tuples for you
#else
  , generalizableOK n ((),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),(),(),(),())
#endif

  -- Silly test, as it basically replicates the actual implementation:
  , backgroundOf int =$ sort $= [ value "==" $ (==) -:> int
                                , value "/=" $ (/=) -:> int
                                , value "<=" $ (<=) -:> int
                                , value "<"  $ (<)  -:> int
                                ]

  , background (mayb int)
    == [ value "=="   ((==) -:> mayb int)
       , value "/="   ((/=) -:> mayb int)
       , value "<="   ((<=) -:> mayb int)
       , value "<"    ((<)  -:> mayb int)
       , value "Just" (Just ->: mayb int) ]

  , background (eith int char)
    == [ value "=="    ((==)  -:> eith int char)
       , value "/="    ((/=)  -:> eith int char)
       , value "<="    ((<=)  -:> eith int char)
       , value "<"     ((<)   -:> eith int char)
       , value "Left"  (Left  ->: eith int char)
       , value "Right" (Right ->: eith int char) ]

  , background [int]
    == [ value "=="     ((==)   -:> [int])
       , value "/="     ((/=)   -:> [int])
       , value "<="     ((<=)   -:> [int])
       , value "<"      ((<)    -:> [int])
       , value "length" (length -:> [int])
       , value "elem"   (elem  ->:> [int]) ]

  , background (mayb nord)
    == [ value "Just" (Just ->: mayb nord) ]

  , background (eith nord nord)
    == [ value "Left"  (Left  ->: eith nord nord)
       , value "Right" (Right ->: eith nord nord) ]

  , background [nord]
    == [ value "length" (length -:> [nord]) ]

  -- background tests
  , listBackgroundOK ()
  , listBackgroundOK int
  , listBackgroundOK bool
  , listBackgroundOK [(mayb (),integer,mayb char)]

  , maybeBackgroundOK ()
  , maybeBackgroundOK integer
  , maybeBackgroundOK char
  , maybeBackgroundOK [(int,bool,[mayb (char,())])]

  , int              `instancesSubset` [mayb [int]]
  , bool             `instancesSubset` ((),bool,char)
  , [mayb ((),char)] `instancesSubset` [(int,(char,[mayb ((),char)]),bool)]
  , not $     bool `instancesSubset` [mayb [int]]
  , not $     bool `bgSubset`        [mayb [int]]
  , not $     bool `sameNamesIn`     [mayb [int]]
  , not $     bool `sameTiersIn`     [mayb [int]]
  , not $ mayb int `instancesSubset` [mayb [int]]
  , not $ mayb int `bgSubset`        [mayb [int]]
  , not $ mayb int `sameNamesIn`     [mayb [int]]
  , not $ mayb int `sameTiersIn`     [mayb [int]]

  , generalizations (instances (undefined :: [Int]) []) [expr [0,0::Int]]
    == map (:[])
       [ _is
       , _i -:- _is
       , _i -:- _i -:- _is
       , _i -:- _i -:- ll
       , _i -:- zero -:- _is
       , _i -:- zero -:- ll
       , zero -:- _is
       , zero -:- _i -:- _is
       , zero -:- _i -:- ll
       , zero -:- zero -:- _is
       ]

  , [ canonicalizeWith (instances (undefined :: [Int]) []) g'
    | g <- generalizations (instances (undefined :: [Int]) []) [expr [0,0::Int]]
    , g' <- vassignments g ]
    == map (:[])
       [ _is
       , _i -:- _is
       , _i -:- _i -:- _is
       , xx -:- xx -:- _is
       , _i -:- _i -:- ll
       , xx -:- xx -:- ll
       , _i -:- zero -:- _is
       , _i -:- zero -:- ll
       , zero -:- _is
       , zero -:- _i -:- _is
       , zero -:- _i -:- ll
       , zero -:- zero -:- _is
       ]

--, holds n $ bgEqOrdOK -:> () -- no Eq or Ord instance on background
  , holds n $ bgEqOrdOK -:> int
  , holds n $ bgEqOK    -:> bool
  , holds n $ bgEqOrdOK -:> char
  , holds n $ bgEqOrdOK -:> integer
  , holds n $ bgEqOrdOK -:> ordering

  , holds n $ bgEqOrdOK -:> (int,int)
  , holds n $ bgEqOK    -:> (int,bool)
  , holds n $ bgEqOK    -:> (bool,int)
  , holds n $ bgEqOK    -:> (bool,bool)
  , holds n $ bgEqOrdOK -:> (char,ordering)

  , holds n $ bgEqOrdOK -:> (int,int,int)
  , holds n $ bgEqOK    -:> (int,int,bool)
  , holds n $ bgEqOK    -:> (int,bool,int)
  , holds n $ bgEqOK    -:> (int,bool,bool)
  , holds n $ bgEqOK    -:> (bool,int,int)
  , holds n $ bgEqOK    -:> (bool,int,bool)
  , holds n $ bgEqOK    -:> (bool,bool,int)
  , holds n $ bgEqOK    -:> (bool,bool,bool)
  , holds n $ bgEqOrdOK -:> (integer,char,ordering)

  , holds n $ bgEqOrdOK -:> (int,int,int,int)
  , holds n $ bgEqOrdOK -:> (integer,char,ordering,int)
  , holds n $ bgEqOK    -:> (bool,bool,bool,bool)

  , holds n $ bgEqOrdOK -:> [int]
  , holds n $ bgEqOK    -:> [bool]
  , holds n $ bgEqOrdOK -:> [char]
  , holds n $ bgEqOrdOK -:> [integer]

  , holds n $ bgEqOrdOK -:> mayb int
  , holds n $ bgEqOK    -:> mayb bool
  , holds n $ bgEqOrdOK -:> mayb char

  , holds n $ bgEqOrdOK -:> eith int int
  , holds n $ bgEqOK    -:> eith int bool
  , holds n $ bgEqOK    -:> eith bool int
  , holds n $ bgEqOK    -:> eith bool bool
  , holds n $ bgEqOrdOK -:> eith char ordering

  , tinstancesUnrepeated $ bool
  , tinstancesUnrepeated $ bool >- bool
  , tinstancesUnrepeated $ int  >- bool
  , tinstancesUnrepeated $ char >- bool
  , tinstancesUnrepeated $ bool >- bool >- bool
  , tinstancesUnrepeated $ int  >- int  >- bool
  , tinstancesUnrepeated $ char >- char >- bool
  , tinstancesUnrepeated $ [bool] >- bool
  , tinstancesUnrepeated $ [int]  >- bool
  , tinstancesUnrepeated $ [char] >- bool
  , tinstancesUnrepeated $ mayb bool >- bool
  , tinstancesUnrepeated $ mayb int  >- bool
  , tinstancesUnrepeated $ mayb char >- bool
  , tinstancesUnrepeated $ eith bool bool >- bool
  , tinstancesUnrepeated $ eith int  int  >- bool
  , tinstancesUnrepeated $ eith char char >- bool
  , tinstancesUnrepeated $ eith char ()   >- bool
  , tinstancesUnrepeated $ ([bool],[bool]) >- bool
  , tinstancesUnrepeated $ ([int], [int])  >- bool
  , tinstancesUnrepeated $ ([char],[char]) >- bool
  , tinstancesUnrepeated $ eith [bool] [bool] >- ([bool],[bool]) >- bool
  ]

tinstancesUnrepeated :: Testable a => a -> Bool
tinstancesUnrepeated p = nub is == is
                      && nub is' == is'
  where
  is = map extractiers $ filter (not . isBackground) $ tinstances p
  is' = map extractiers $ filter (not . isBackground) $ fullInstances p
  -- All tiers look the same without evaluating its elements:
  -- > > tinstances bool
  -- > [tiers :: [[Expr]],name :: Bool -> [Char],background :: [Expr],tiers :: [[Expr]],name :: Int -> [Char],background :: [Expr]]
  -- So, this takes a representative to use in the comparison:
  extractiers e@(Value "tiers" _)  =  head $ concat (eval undefined e :: [[Expr]])
  extractiers e                    =  e
  -- every background looks the same without evaluating its elements
  isBackground (Value "background" _)  =  True
  isBackground _                       =  False

listBackgroundOK :: Generalizable a => a -> Bool
listBackgroundOK x = backgroundListOf x `subset` backgroundOf [x]
  where
  backgroundListOf x = [ value "length" $ length  -:> [x] ]
                     +++ backgroundOf x

maybeBackgroundOK :: Generalizable a => a -> Bool
maybeBackgroundOK x = backgroundMaybeOf x `subset` backgroundOf (mayb x)
  where
  backgroundMaybeOf x = [value "Just" $ Just -:> x] +++ backgroundOf x