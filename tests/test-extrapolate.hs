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
  ]
