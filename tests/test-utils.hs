-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Test.Extrapolate.Utils

main :: IO ()
main = mainTest tests 1000

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ elemBy (==) ==== elem -:> ()
  , holds n $ elemBy (==) ==== elem -:> int
  , holds n $ elemBy (==) ==== elem -:> bool
  , holds n $ elemBy (==) ==== elem -:> [int]
  , holds n $ elemBy (==) ==== elem -:> [bool]
  ]
