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
  , holds n $ generalizableOK -:> ()
  , holds n $ generalizableOK -:> int
  , holds n $ generalizableOK -:> integer
  , holds n $ generalizableOK -:> bool
  , holds n $ generalizableOK -:> char
  , holds 9 $ generalizableOK -:> [()]
  , holds n $ generalizableOK -:> [int]
  , holds n $ generalizableOK -:> [integer]
  , holds n $ generalizableOK -:> [bool]
  , holds n $ generalizableOK -:> [char]
  , holds n $ generalizableOK -:> (mayb ())
  , holds n $ generalizableOK -:> (mayb int)
  , holds n $ generalizableOK -:> (mayb integer)
  , holds n $ generalizableOK -:> (mayb bool)
  , holds n $ generalizableOK -:> (mayb char)
  , holds n $ generalizableOK -:> (int,bool)
  , holds n $ generalizableOK -:> ((),integer)
  , holds n $ generalizableOK -:> ((),bool,integer)
-- TODO: implement further tuple instances (4,5,6) and uncomment below
--, holds n $ generalizableOK -:> (int,(),bool,integer)
--, holds n $ generalizableOK -:> (int,(),bool,integer,char)
--, holds n $ generalizableOK -:> (string,int,(),bool,integer,char)
-- TODO: implement further tuple instances (7,8,9,10,11,12) and uncomment below
--, holds n $ generalizableOK -:> ((),(),(),(),(),(),())
--, holds n $ generalizableOK -:> ((),(),(),(),(),(),(),())
--, holds n $ generalizableOK -:> ((),(),(),(),(),(),(),(),())
--, holds n $ generalizableOK -:> ((),(),(),(),(),(),(),(),(),())
--, holds n $ generalizableOK -:> ((),(),(),(),(),(),(),(),(),(),())
--, holds n $ generalizableOK -:> ((),(),(),(),(),(),(),(),(),(),(),())

  -- Properties about the "instances" function.
  , instancesOK ()
  , instancesOK int
  , instancesOK integer
  , instancesOK bool
  , instancesOK char
  , instancesOK [()]
  , instancesOK [int]
  , instancesOK [integer]
  , instancesOK [bool]
  , instancesOK [char]
  , instancesOK (mayb ())
  , instancesOK (mayb int)
  , instancesOK (mayb integer)
  , instancesOK (mayb bool)
  , instancesOK (mayb char)
  , instancesOK (int,bool)
  , instancesOK ((),integer)
  , instancesOK ((),bool,integer)
-- TODO: implement further tuple instances (4,5,6) and uncomment below
--, instancesOK (int,(),bool,integer)
--, instancesOK (int,(),bool,integer,char)
--, instancesOK (string,int,(),bool,integer,char)
-- TODO: implement further tuple instances (7,8,9,10,11,12) and uncomment below
--, instancesOK ((),(),(),(),(),(),())
--, instancesOK ((),(),(),(),(),(),(),())
--, instancesOK ((),(),(),(),(),(),(),(),())
--, instancesOK ((),(),(),(),(),(),(),(),(),())
--, instancesOK ((),(),(),(),(),(),(),(),(),(),())
--, instancesOK ((),(),(),(),(),(),(),(),(),(),(),())

  -- Silly test, as it basically replicates the actual implementation:
  , backgroundOf int =$ sort $= [ constant "==" $ (==) -:> int
                                , constant "/=" $ (/=) -:> int
                                , constant "<=" $ (<=) -:> int
                                , constant "<"  $ (<)  -:> int
                                ]

  -- background tests
  , listBackgroundOK ()
  , listBackgroundOK int
  , listBackgroundOK integer
  , listBackgroundOK bool
  , listBackgroundOK char
  , listBackgroundOK [()]
  , listBackgroundOK [int]
  , listBackgroundOK [integer]
  , listBackgroundOK [bool]
  , listBackgroundOK [char]
  , listBackgroundOK (mayb ())
  , listBackgroundOK (mayb int)
  , listBackgroundOK (mayb integer)
  , listBackgroundOK (mayb bool)
  , listBackgroundOK (mayb char)

  , maybeBackgroundOK ()
  , maybeBackgroundOK int
  , maybeBackgroundOK integer
  , maybeBackgroundOK bool
  , maybeBackgroundOK char
  , maybeBackgroundOK [()]
  , maybeBackgroundOK [int]
  , maybeBackgroundOK [integer]
  , maybeBackgroundOK [bool]
  , maybeBackgroundOK [char]
  , maybeBackgroundOK (mayb ())
  , maybeBackgroundOK (mayb int)
  , maybeBackgroundOK (mayb integer)
  , maybeBackgroundOK (mayb bool)
  , maybeBackgroundOK (mayb char)
  ]

listBackgroundOK :: Generalizable a => a -> Bool
listBackgroundOK x = backgroundOf [x] =$ sort $= backgroundListOf x
  where
  backgroundListOf x = [ constant "length" $ length  -:> [x]
                       , constant "filter" $ filter ->:> [x]
                       ]
                     +++ backgroundOf x

maybeBackgroundOK :: Generalizable a => a -> Bool
maybeBackgroundOK x = backgroundOf (mayb x) =$ sort $= backgroundMaybeOf x
  where
  backgroundMaybeOf x = [constant "Just" $ Just -:> x] +++ backgroundOf x
