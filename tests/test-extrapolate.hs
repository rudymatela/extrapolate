-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

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
  , holds n $ \x -> show (expr x) == show (x :: Integer) ++ " :: Integer"

  , holds 9 $ \xs -> show (expr xs) == show (xs :: [()]) ++ " :: [()]"
  , holds n $ \xs -> show (expr xs) == show (xs :: [Int]) ++ " :: [Int]"
  , holds n $ \ps -> show (expr ps) == show (ps :: [Bool]) ++ " :: [Bool]"
  , holds n $ \xs -> show (expr xs) == show (xs :: [Integer]) ++ " :: [Integer]"

  , holds n $ \mx -> show (expr mx) == show (mx :: Maybe ()) ++ " :: Maybe ()"
  , holds n $ \mx -> show (expr mx) == show (mx :: Maybe Int) ++ " :: Maybe Int"
  , holds n $ \mp -> show (expr mp) == show (mp :: Maybe Bool) ++ " :: Maybe Bool"
  , holds n $ \mx -> show (expr mx) == show (mx :: Maybe Integer) ++ " :: Maybe Integer"

  , holds n $ \xy -> show (expr xy) == show (xy :: ((),Int)) ++ " :: ((),Int)"
  , holds n $ \xy -> show (expr xy) == show (xy :: (Bool,Integer)) ++ " :: (Bool,Integer)"
  , holds n $ \xyz -> show (expr xyz) == show (xyz :: ((),Int,Bool)) ++ " :: ((),Int,Bool)"
-- TODO: implement further tuple instances (4,5,6) and uncomment below
--, holds n $ \xyzw -> show (expr xyzw)
--                  == show (xyzw :: ((),Int,Integer,Bool)) ++ " :: ((),Int,Integer,Bool)"
--, holds n $ \xyzwv -> show (expr xyzwv)
--                   == show (xyzwv :: ((),Int,Integer,Bool,())) ++ " :: ((),Int,Integer,Bool,())"
--, holds n $ \xyzwvu -> show (expr xyzwvu)
--                    == show (xyzwvu :: ((),Int,Integer,Bool,(),Int)) ++ " :: ((),Int,Integer,Bool,(),Int)"
  ]
