{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (isPrefixOf)
import Data.Typeable (typeOf)
import Test.Speculate.Expr.Instance as I

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable List
deriving instance Typeable Perhaps
deriving instance Typeable Ship
deriving instance Typeable Arrangement
deriving instance Typeable NonEmptyList
deriving instance Typeable Mutual
deriving instance Typeable Shared
deriving instance Typeable Tree
deriving instance Typeable Leafy
deriving instance Typeable Dict
#endif

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
              | End
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


deriveGeneralizable ''List
{- -- derivation:
instance (Generalizable a) => Generalizable (List a) where
  expr xs@Nil          =  constant "Nil"  (Nil    -: xs)
  expr xs@(Cons y ys)  =  constant "Cons" (Cons ->>: xs) :$ expr y :$ expr ys
  instances xs = this "xs" xs
               $ instances (argTy1of1 xs)
-- note the use of -: and ->>: instead of argTypes<N>
-}

deriveGeneralizable ''Perhaps
{- -- derivation:
instance (Generalizable a) => Generalizable (Perhaps a) where
  expr px@Naught      =  constant "Naught" (Naught  -: px)
  expr px@(Simply x)  =  constant "Simply" (Simply ->: px) :$ expr x
  instances px = this "px" px
               $ instances (argTy1of1 px)
-}

deriveGeneralizable ''Ship
{- -- derivation:
instance (Generalizable a, Generalizable b) => Generalizable (Ship a b) where
  expr s@(Port x)       =  constant "Port"      (Port      ->: s) :$ expr x
  expr s@(Starboard y)  =  constant "Starboard" (Starboard ->: s) :$ expr y
  instances s = this "s" s
              $ instances (argTy1of2 s)
              . instances (argTy2of2 s)
-}

-- deriveGeneralizable ''Arrangement -- TODO: make this work

deriveGeneralizable ''NonEmptyList

deriveGeneralizable ''Mutual
deriveGeneralizable ''Shared

deriveGeneralizable ''Tree
deriveGeneralizable ''Leafy

deriveGeneralizable ''Dict


main :: IO ()
main = mainTest tests 2160

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ generalizableOK -:> ls bool
  , holds n $ generalizableOK -:> perhaps int
  , holds n $ generalizableOK -:> ship int char
--, holds n $ generalizableOK -:> arrangement -- TODO: make this work
  , holds n $ generalizableOK -:> mutual bool
  , holds n $ generalizableOK -:> shared int
  , holds n $ generalizableOK -:> tree bool
  , holds n $ generalizableOK -:> leafy int
  , holds n $ generalizableOK -:> dict bool int

  , instancesOK (ls int)
  , instancesOK (perhaps bool)
  , instancesOK (ship bool ())
--, instancesOK (arrangement) -- TODO: make this work
  , instancesOK (mutual int)
  , instancesOK (shared bool)
  , instancesOK (tree int)
  , instancesOK (leafy bool)
  , instancesOK (dict int bool)

  ,       int  `sameTiersIn` ls int
  , not $ bool `sameTiersIn` ls int
  ,       ()   `sameTiersIn` perhaps ()
  , not $ ()   `sameTiersIn` perhaps int
  ,       char `sameTiersIn` ship char int
  ,       int  `sameTiersIn` ship char int
  , not $ bool `sameTiersIn` ship char int
  ,       int  `sameTiersIn` mutual int
  ,       int  `sameTiersIn` shared int
  ,       bool `sameTiersIn` tree bool
  ,       int  `sameTiersIn` leafy int
  ,       int  `sameTiersIn` dict int bool
  ,       bool `sameTiersIn` dict int bool
  ,       bool `sameTiersIn` dict int (perhaps (ship char bool))
  , shared (ship (ls char) bool)
    `sameTiersIn` dict int (perhaps (mutual (ship (ls char) bool)))
  ]


generalizableOK :: (Eq a, Show a, Generalizable a) => a -> Bool
generalizableOK = idExprEval &&& showOK

idExprEval :: (Eq a, Generalizable a) => a -> Bool
idExprEval x = eval (error "idExprEval: could not eval") (expr x) == x

showOK :: (Show a, Generalizable a) => a -> Bool
showOK x = show x == dropType (show (expr x))
  where
  dropType :: String -> String
  dropType cs     | " :: " `isPrefixOf` cs = ""
  dropType ""     =  ""
  dropType (c:cs) =  c : dropType cs

instancesOK :: (Eq a, Generalizable a) => a -> Bool
instancesOK = namesOK &&& tiersOK

namesOK :: Generalizable a => a -> Bool
namesOK x =  x `sameNamesIn` [x]
          && x `sameNamesIn` [[x]]
          && x `sameNamesIn` mayb x
          && x `sameNamesIn` (x,x)
          && x `sameNamesIn` (x,())
          && x `sameNamesIn` ((),x)
          && x `sameNamesIn` (x,(),())
          && x `sameNamesIn` ((),x,())
          && x `sameNamesIn` ((),(),x)

sameNamesIn :: (Generalizable a, Generalizable b) => a -> b -> Bool
x `sameNamesIn` c = x `namesIn` x
           =| 12 |= x `namesIn` c

namesIn :: (Generalizable a, Generalizable b) => a -> b -> [String]
x `namesIn` c = I.names (instances c []) (typeOf x)

tiersOK :: (Eq a, Generalizable a) => a -> Bool
tiersOK x =  x `sameTiersIn` x
          && x `sameTiersIn` [x]
          && x `sameTiersIn` [[x]]
          && x `sameTiersIn` (mayb x)
          && x `sameTiersIn` (x,x)
          && x `sameTiersIn` (x,())
          && x `sameTiersIn` ((),x)
          && x `sameTiersIn` (x,(),())
          && x `sameTiersIn` ((),x,())
          && x `sameTiersIn` ((),(),x)

sameTiersIn :: (Eq a, Generalizable a, Generalizable b) => a -> b -> Bool
x `sameTiersIn` cx = isListable (instances cx []) (typeOf x)
                  && (tiers -: [[x]]) =| 6 |= tiersIn cx

tiersIn :: (Generalizable a, Generalizable b) => b -> [[a]]
tiersIn c = ret
  where
  ret = mapT (eval . error $ "tiersIn: the imposible happened")
      $ tiersE (instances c []) (typeOf (head (head ret)))
