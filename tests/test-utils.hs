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

  , holds n $ listEq (==) ==== (==) -:> [()]
  , holds n $ listEq (==) ==== (==) -:> [int]
  , holds n $ listEq (==) ==== (==) -:> [bool]
  , holds n $ listEq (==) ==== (==) -:> [[int]]
  , holds n $ listEq (==) ==== (==) -:> [[bool]]

  , holds n $ listOrd (<=) ==== (<=) -:> [()]
  , holds n $ listOrd (<=) ==== (<=) -:> [int]
  , holds n $ listOrd (<=) ==== (<=) -:> [bool]
  , holds n $ listOrd (<=) ==== (<=) -:> [[int]]
  , holds n $ listOrd (<=) ==== (<=) -:> [[bool]]

  , holds n $ maybeEq (==) ==== (==) -:> mayb ()
  , holds n $ maybeEq (==) ==== (==) -:> mayb int
  , holds n $ maybeEq (==) ==== (==) -:> mayb bool
  , holds n $ maybeEq (==) ==== (==) -:> mayb [int]
  , holds n $ maybeEq (==) ==== (==) -:> mayb [bool]

  , holds n $ maybeOrd (<=) ==== (<=) -:> mayb ()
  , holds n $ maybeOrd (<=) ==== (<=) -:> mayb int
  , holds n $ maybeOrd (<=) ==== (<=) -:> mayb bool
  , holds n $ maybeOrd (<=) ==== (<=) -:> mayb [int]
  , holds n $ maybeOrd (<=) ==== (<=) -:> mayb [bool]

  , holds n $ eitherEq (==) (==) ==== (==) -:> eith () ()
  , holds n $ eitherEq (==) (==) ==== (==) -:> eith int int
  , holds n $ eitherEq (==) (==) ==== (==) -:> eith int bool
  , holds n $ eitherEq (==) (==) ==== (==) -:> eith bool int
  , holds n $ eitherEq (==) (==) ==== (==) -:> eith bool bool

  , holds n $ eitherOrd (<=) (<=) ==== (<=) -:> eith () ()
  , holds n $ eitherOrd (<=) (<=) ==== (<=) -:> eith int int
  , holds n $ eitherOrd (<=) (<=) ==== (<=) -:> eith int bool
  , holds n $ eitherOrd (<=) (<=) ==== (<=) -:> eith bool int
  , holds n $ eitherOrd (<=) (<=) ==== (<=) -:> eith bool bool

  , minimumOn fst [(1,6),(2,5),(3,4)] == (1,6)
  , minimumOn snd [(1,6),(2,5),(3,4)] == (3,4)
  ]
