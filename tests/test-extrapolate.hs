-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
  , expr ([]::[Int]) == constant "[]" ([]::[Int])
  , expr ([0::Int])  == zero -:- ll
  , expr ([0::Int,1])  == zero -:- one -:- ll
  , holds n $ \xs -> expr xs == foldr (-:-) ll (map expr (xs :: [Int]))
  , holds n $ \xs -> expr xs == foldr (-:-) llb (map expr (xs :: [Bool]))
  ]
