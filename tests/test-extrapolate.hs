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
  , generalizableOK n (int,(),bool,integer)
--, generalizableOK n (int,(),bool,integer,char)
-- TODO: fix show of 5-uples on Speculate, then replace following for previous
  , holds n $ idExprEval -:> (int,(),bool,integer,char)
  , instancesOK              (int,(),bool,integer,char)
--, generalizableOK n (string,int,(),bool,integer,char)
-- TODO: fix show of 6-uples on Speculate, then replace following for previous
  , holds n $ idExprEval -:> (string,int,(),bool,integer,char)
  , instancesOK              (string,int,(),bool,integer,char)
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

  , candidateConditions (([int] >- bool) `With` MaxConditionSize 3) [xxs]
    == [ true, elem' zero xxs ]

  , candidateConditions (([int] >- bool) `With` MaxConditionSize 3) [xx -:- xxs]
    == [ true
       , elem' zero xxs
       , elem' xx ll
       , elem' xx xxs
       , zero -/=- xx
       , xx   -/=- zero
       , xx   -/=- xx
       , zero -<- xx
       , xx   -<- zero
       , xx   -<- xx
       , zero -<=- xx
       , xx   -<=- zero
       , xx   -<=- xx
       ]
  ]

listBackgroundOK :: Generalizable a => a -> Bool
listBackgroundOK x = backgroundListOf x `subset` backgroundOf [x]
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
