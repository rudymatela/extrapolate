{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

-- List is isomorphic to []
data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq, Ord)

-- Perhaps is isomorphic to Maybe
data Perhaps a = Naught
               | Simply a
  deriving (Show, Eq, Ord)

-- Ship is isomorphic to Either
data Ship a b = Port a
              | Starboard b
  deriving (Show, Eq, Ord)

-- Arrangement is isomorphic to Ordering
data Arrangement = Lesser
                 | Equal
                 | Greater
  deriving (Show, Eq, Ord)

data NonEmptyList a = Buil a (NonEmptyList a)
                    | Unit a
  deriving (Show, Eq, Ord)

data Mutual a = Mutual a (Shared a) | M a deriving (Show, Eq, Ord)
data Shared a = Shared (Mutual a) a | S a deriving (Show, Eq, Ord)

data Tree a  = Node (Tree a) a (Tree a) | Empty
  deriving (Show, Eq, Ord)

data Leafy a = Branch (Leafy a) (Leafy a) | Leaf a
  deriving (Show, Eq, Ord)

data Dict a b = Meaning a b (Dict a b)
  deriving (Show, Eq, Ord)


-- TODO: auto-derive Listable from deriveGeneralizable?
deriveListable ''List
deriveListable ''Perhaps
deriveListable ''Ship
deriveListable ''Arrangement

deriveListable ''NonEmptyList

deriveListable ''Mutual
deriveListable ''Shared

deriveListable ''Tree
deriveListable ''Leafy

deriveListable ''Dict


-- deriveGeneralizable ''List         -- TODO: make this work
-- deriveGeneralizable ''Perhaps      -- TODO: make this work
-- deriveGeneralizable ''Ship         -- TODO: make this work
-- deriveGeneralizable ''Arrangement  -- TODO: make this work

deriveGeneralizable ''NonEmptyList

deriveGeneralizable ''Mutual
deriveGeneralizable ''Shared

-- deriveGeneralizable ''Tree         -- TODO: make this work
deriveGeneralizable ''Leafy

deriveGeneralizable ''Dict


main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
-- TODO: add tests of isomorphicity
  ]
