{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
-- Copyright (c) 2017-2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (sort)

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
deriving instance Typeable Data
deriving instance Typeable EqData
deriving instance Typeable OrdData
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

data Data    = Data    deriving (Show)
data EqData  = EqData  deriving (Show, Eq)
data OrdData = OrdData deriving (Show, Eq, Ord)


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


deriveListable ''List
deriveListable ''Perhaps
deriveListable ''Ship


deriveGeneralizable ''List
{- -- derivation:
instance (Generalizable a) => Generalizable (List a) where
  name _ = "xs"
  expr xs@Nil          =  value "Nil"  (Nil    -: xs)
  expr xs@(Cons y ys)  =  value "Cons" (Cons ->>: xs) :$ expr y :$ expr ys
  instances xs = this xs
               $ (let Cons y ys = Cons undefined undefined -: xs
                  in instances y . instances ys)

-- It may seem like its possible to derive just:
--instances xs = this xs
--             $ instances (argTy1of1 xs)
-- However that will restrain us from recursing into non argument types that
-- need to be present (cf. the Mutual & Shared types).
-}

deriveGeneralizable ''Perhaps
{- -- derivation:
instance (Generalizable a) => Generalizable (Perhaps a) where
  expr px@Naught      =  value "Naught" (Naught  -: px)
  expr px@(Simply x)  =  value "Simply" (Simply ->: px) :$ expr x
  instances px = ...
-}

deriveGeneralizable ''Ship
{- -- derivation:
instance (Generalizable a, Generalizable b) => Generalizable (Ship a b) where
  expr s@(Port x)       =  value "Port"      (Port      ->: s) :$ expr x
  expr s@(Starboard y)  =  value "Starboard" (Starboard ->: s) :$ expr y
  instances s = ...
-}

deriveGeneralizable ''Arrangement

deriveGeneralizable ''NonEmptyList

deriveGeneralizableCascading ''Mutual

deriveGeneralizable ''Tree
deriveGeneralizable ''Leafy

deriveGeneralizable ''Dict

deriveGeneralizable ''Data
deriveGeneralizable ''EqData
deriveGeneralizable ''OrdData

{-
-- these should produce warnings because instances already exist:
deriveGeneralizable ''Int
deriveGeneralizable ''Bool
deriveGeneralizable ''Either
-}

-- these should /not/ produce warnings even though instances already exist:
deriveGeneralizableIfNeeded ''Int
deriveGeneralizableIfNeeded ''Bool
deriveGeneralizableIfNeeded ''Either


main :: IO ()
main = mainTest tests 2160

tests :: Int -> [Bool]
tests n =
  [ True

  , generalizableOK n $ ls bool
  , generalizableOK n $ perhaps int
  , generalizableOK n $ ship int char
  , generalizableOK n $ arrangement
  , generalizableOK n $ nonEmptyList int
  , generalizableOK n $ mutual bool
  , generalizableOK n $ shared int
  , generalizableOK n $ tree bool
  , generalizableOK n $ leafy int
  , generalizableOK n $ dict bool int
--, generalizableOK n $ Data -- not Eq, can't run test
  , generalizableOK n $ EqData
  , generalizableOK n $ OrdData

  ,       int  `instancesSubset` ls int
  , not $ bool `instancesSubset` ls int
  ,       ()   `instancesSubset` perhaps ()
  , not $ ()   `instancesSubset` perhaps int
  ,       ()   `bgSubset`        perhaps int -- () has no background
  ,       char `instancesSubset` ship char int
  ,       int  `instancesSubset` ship char int
  , not $ bool `instancesSubset` ship char int
  ,       int  `instancesSubset` mutual int
  ,       int  `instancesSubset` shared int
  , shared int `instancesSubset` mutual int
  , mutual ()  `instancesSubset` shared ()
  ,       bool `instancesSubset` tree bool
  ,       int  `instancesSubset` leafy int
  ,       int  `instancesSubset` dict int bool
  ,       bool `instancesSubset` dict int bool
  ,       bool `instancesSubset` dict int (perhaps (ship char bool))
  , shared (ship (ls char) bool)
    `instancesSubset` dict int (perhaps (mutual (ship (ls char) bool)))

  , backgroundOf Data    =$ sort $= []
  , backgroundOf EqData  =$ sort $= [ value "==" $ (==) -:> EqData
                                    , value "/=" $ (/=) -:> EqData
                                    ]
  , backgroundOf OrdData =$ sort $= [ value "==" $ (==) -:> OrdData
                                    , value "/=" $ (/=) -:> OrdData
                                    , value "<=" $ (<=) -:> OrdData
                                    , value "<"  $ (<)  -:> OrdData
                                    ]
  ]
