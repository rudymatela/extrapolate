-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.List (sort)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  -- Transforming lists into Exprs
  , expr ([]::[Int]) == constant "[]" ([]::[Int])
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
  , expr ((0,False) :: (Int,Bool))  ==  zero `comma` false
  , expr ((True,1)  :: (Bool,Int))  ==  true `comma` one

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
-- TODO: implement further tuple instances (4,5,6) and uncomment below
--, generalizableOK n (int,(),bool,integer)
--, generalizableOK n (int,(),bool,integer,char)
--, generalizableOK n (string,int,(),bool,integer,char)
-- TODO: implement further tuple instances (7,8,9,10,11,12) and uncomment below
--, generalizableOK n ((),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),(),(),())
--, generalizableOK n ((),(),(),(),(),(),(),(),(),(),(),())

  -- Silly test, as it basically replicates the actual implementation:
  , backgroundOf int =$ sort $= [ constant "==" $ (==) -:> int
                                , constant "/=" $ (/=) -:> int
                                , constant "<=" $ (<=) -:> int
                                , constant "<"  $ (<)  -:> int
                                ]

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
  ]

listBackgroundOK :: Generalizable a => a -> Bool
listBackgroundOK x = backgroundOf [x] =$ sort $= backgroundListOf x
  where
  backgroundListOf x = [ constant "length" $ length  -:> [x]
                       , constant "any"    $ any    ->:> [x]
                       , constant "all"    $ all    ->:> [x]
                       , constant "filter" $ filter ->:> [x]
                       ]
                     +++ backgroundOf x

maybeBackgroundOK :: Generalizable a => a -> Bool
maybeBackgroundOK x = backgroundOf (mayb x) =$ sort $= backgroundMaybeOf x
  where
  backgroundMaybeOf x = [constant "Just" $ Just -:> x] +++ backgroundOf x
