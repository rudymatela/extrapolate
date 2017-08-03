{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (isPrefixOf)

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


-- Dummy undefined values --

ls :: a -> List a
ls = undefined

perhaps :: a -> Perhaps a
perhaps = undefined

ship :: a -> b -> Ship a b
ship = undefined

arrangement :: Arrangement
arrangement = undefined

nonEmptyList :: a -> NonEmptyList a
nonEmptyList = undefined

mutual :: a -> Mutual a
mutual = undefined

shared :: a -> Shared a
shared = undefined

tree :: a -> Tree a
tree = undefined

leafy :: a -> Leafy a
leafy = undefined

dict :: a -> b -> Dict a b
dict = undefined


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


-- deriveGeneralizable ''List         -- TODO: make this work, target:

instance (Generalizable a) => Generalizable (List a) where
  expr xs@Nil          =  constant "Nil"  (Nil    -: xs)
  expr xs@(Cons y ys)  =  constant "Cons" (Cons ->>: xs) :$ expr y :$ expr ys
  instances xs = this "xs" xs
               $ instances (argTy1of1 xs)
-- note the use of -: and ->>: instead of argTypes<N>

-- deriveGeneralizable ''Perhaps      -- TODO: make this work
instance (Generalizable a) => Generalizable (Perhaps a) where
  expr px@Naught      =  constant "Naught" (Naught  -: px)
  expr px@(Simply x)  =  constant "Simply" (Simply ->: px) :$ expr x
  instances px = this "px" px
               $ instances (argTy1of1 px)

-- deriveGeneralizable ''Ship         -- TODO: make this work
instance (Generalizable a, Generalizable b) => Generalizable (Ship a b) where
  expr s@(Port x)       =  constant "Port"      (Port      ->: s) :$ expr x
  expr s@(Starboard y)  =  constant "Starboard" (Starboard ->: s) :$ expr y
  instances s = this "s" s
              $ instances (argTy1of2 s)
              . instances (argTy2of2 s)

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

  , holds n $ generalizableOK -:> ls int
  , holds n $ generalizableOK -:> ls bool
  , holds n $ generalizableOK -:> perhaps int
  , holds n $ generalizableOK -:> perhaps bool
  , holds n $ generalizableOK -:> ship int int
  , holds n $ generalizableOK -:> ship bool ()
--, holds n $ generalizableOK -:> arrangement -- TODO: make this work

-- TODO: add tests of isomorphicity
  ]


generalizableOK :: (Eq a, Show a, Generalizable a) => a -> Bool
generalizableOK x = idExprEval x && showOK x

idExprEval :: (Eq a, Generalizable a) => a -> Bool
idExprEval x = eval (error "idExprEval: could not eval") (expr x) == x

showOK :: (Show a, Generalizable a) => a -> Bool
showOK x = show x == dropType (show (expr x))
  where
  dropType :: String -> String
  dropType cs     | " :: " `isPrefixOf` cs = ""
  dropType ""     =  ""
  dropType (c:cs) =  c : dropType cs
