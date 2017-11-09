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

  , holds n $ pairEq (==) (==) ==== (==) -:> ((),())
  , holds n $ pairEq (==) (==) ==== (==) -:> (int,int)
  , holds n $ pairEq (==) (==) ==== (==) -:> (int,bool)
  , holds n $ pairEq (==) (==) ==== (==) -:> (bool,int)
  , holds n $ pairEq (==) (==) ==== (==) -:> (bool,bool)

  , holds n $ pairOrd (<=) (<=) ==== (<=) -:> ((),())
  , holds n $ pairOrd (<=) (<=) ==== (<=) -:> (int,int)
  , holds n $ pairOrd (<=) (<=) ==== (<=) -:> (int,bool)
  , holds n $ pairOrd (<=) (<=) ==== (<=) -:> (bool,int)
  , holds n $ pairOrd (<=) (<=) ==== (<=) -:> (bool,bool)

  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> ((),(),())
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (int,int,int)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (int,int,bool)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (int,bool,int)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (int,bool,bool)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (bool,int,int)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (bool,int,bool)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (bool,bool,int)
  , holds n $ tripleEq (==) (==) (==) ==== (==) -:> (bool,bool,bool)

  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> ((),(),())
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (int,int,int)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (int,int,bool)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (int,bool,int)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (int,bool,bool)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (bool,int,int)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (bool,int,bool)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (bool,bool,int)
  , holds n $ tripleOrd (<=) (<=) (<=) ==== (<=) -:> (bool,bool,bool)

  , minimumOn fst [(1,6),(2,5),(3,4)] == (1,6)
  , minimumOn snd [(1,6),(2,5),(3,4)] == (3,4)
  , maximumOn fst [(1,6),(2,5),(3,4)] == (3,4)
  , maximumOn snd [(1,6),(2,5),(3,4)] == (1,6)
  , maximumOn fst [(1,6),(1,5),(1,4)] == (1,6)
  ]
