{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8

-- Copied from SmartCheck's examples,
-- Which in turn was,
-- Copied from QuickCheck2's examples.

import Data.List (sort)
import Data.Typeable
import Test.Extrapolate
import qualified Test.LeanCheck as Lean

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable Heap
deriving instance Typeable HeapP
deriving instance Typeable HeapPP
#endif


instance (Ord a, Listable a) => Listable (Heap a) where
  tiers = cons1 heap

instance Name (Heap a) where
  name _ = "h"

instance (Ord a, Express a) => Express (Heap a) where
  expr h = value "fromList" (fromList ->: h) :$ expr (toList h)

instance (Ord a, Generalizable a) => Generalizable (Heap a) where
  background h = [ value "size" $ size -:> h ]
  instances h = this h $ instances (toList h)


data Heap a
  = Node a (Heap a) (Heap a)
  | Nil
 deriving (Eq, Ord, Show, Read)

empty :: Heap a
empty = Nil

isEmpty :: Heap a -> Bool
isEmpty Nil = True
isEmpty _   = False

unit :: a -> Heap a
unit x = Node x empty empty

size :: Heap a -> Int
size Nil            = 0
size (Node _ h1 h2) = 1 + size h1 + size h2

insert :: Ord a => a -> Heap a -> Heap a
insert x h = unit x `merge` h

removeMin :: Ord a => Heap a -> Maybe (a, Heap a)
removeMin Nil            = Nothing
removeMin (Node x h1 h2) = Just (x, h1 `merge` h2)

merge :: Ord a => Heap a -> Heap a -> Heap a
h1  `merge` Nil = h1
Nil `merge` h2  = h2
h1@(Node x h11 h12) `merge` h2@(Node y h21 h22)
  | x <= y    = Node x (h12 `merge` h2) h11
  | otherwise = Node y (h22 `merge` h1) h21

fromList :: Ord a => [a] -> Heap a
fromList xs = merging [ unit x | x <- xs ]
 where
  merging []  = empty
  merging [h] = h
  merging hs  = merging (sweep hs)

  sweep []         = []
  sweep [h]        = [h]
  sweep (h1:h2:hs) = (h1 `merge` h2) : sweep hs

toList :: Heap a -> [a]
toList h = toList' [h]
 where
  toList' []                  = []
  toList' (Nil          : hs) = toList' hs
  toList' (Node x h1 h2 : hs) = x : toList' (h1:h2:hs)

toSortedList :: Ord a => Heap a -> [a]
toSortedList Nil            = []
toSortedList (Node x h1 h2) = x : toList (h1 `merge` h2)
-- above, toList should instead read toSortedList


data HeapP a
  = Empty
  | Unit a
  | Insert a (HeapP a)
  | SafeRemoveMin (HeapP a)
  | Merge (HeapP a) (HeapP a)
  | FromList [a]
 deriving (Show, Read)

heap :: Ord a => HeapP a -> Heap a
heap Empty             = empty
heap (Unit x)          = unit x
heap (Insert x p)      = insert x (heap p)
heap (SafeRemoveMin p) = case removeMin (heap p) of
                           Nothing    -> empty -- arbitrary choice
                           Just (_,h) -> h
heap (Merge p q)       = heap p `merge` heap q
heap (FromList xs)     = fromList xs

instance (Listable a) => Listable (HeapP a) where
  tiers  =  cons0 Empty
         \/ cons1 Unit
         \/ cons2 Insert
         \/ cons1 SafeRemoveMin
         \/ cons2 Merge
         \/ cons1 FromList

instance Name (HeapP a) where
  name _ = "p"

instance Express a => Express (HeapP a) where
  expr p'@Empty             = value "Empty"    (Empty     -: p')
  expr p'@(Unit x)          = value "Unit"     (Unit     ->: p') :$ expr x
  expr p'@(Insert x p)      = value "Insert"   (Insert  ->>: p') :$ expr x :$ expr p
  expr p'@(SafeRemoveMin x) = value "SafeRemoveMin" (SafeRemoveMin ->: p') :$ expr x
  expr p'@(Merge p q)       = value "Merge"    (Merge   ->>: p') :$ expr p :$ expr q
  expr p'@(FromList xs)     = value "FromList" (FromList ->: p') :$ expr xs

instance (Generalizable a, Typeable a) => Generalizable (HeapP a) where
  instances p = this p
              $ let Unit x = Unit undefined `asTypeOf` p
                in instances x



data HeapPP a = HeapPP { program :: (HeapP a)
                       , theHeap :: (Heap a) }
  deriving (Show, Read)

instance (Ord a, Listable a) => Listable (HeapPP a) where
  tiers = cons1 heappp

heappp :: Ord a => HeapP a -> HeapPP a
heappp p  =  HeapPP p (heap p)

instance Name (HeapPP a) where
  name _ = "hpp"

instance (Ord a, Express a) => Express (HeapPP a) where
  expr (HeapPP p _) = value "heappp" (heappp -:> p) :$ expr p

instance (Ord a, Generalizable a) => Generalizable (HeapPP a) where
  background hpp = [ value "program" $ program -:> hpp
                   , value "theHeap" $ theHeap -:> hpp
                   ]
  instances hpp = this hpp
                $ let HeapPP p h = HeapPP undefined undefined `asTypeOf` hpp
                  in instances p . instances h


(==?) :: Ord a => Heap a -> [a] -> Bool
h ==? xs = sort (toList h) == sort xs

prop_ToSortedList :: Ord a => HeapPP a -> Bool
prop_ToSortedList (HeapPP _ h) =
  h ==? xs && xs == sort xs
 where
  xs = toSortedList h

sizePP :: HeapPP a -> Int
sizePP (HeapPP h0 h1) = sizeP h0 + sizeH h1

sizeP :: HeapP a -> Int
sizeP hp = case hp of
  Empty           -> 1
  Unit _          -> 1
  Insert _ h      -> 1 + sizeP h
  SafeRemoveMin h -> 1 + sizeP h
  Merge h0 h1     -> 1 + sizeP h0 + sizeP h1
  FromList ls     -> 1 + length ls

sizeH :: Heap a -> Int
sizeH hp = case hp of
  Node a h0 h1 -> 1 + sizeH h0 + sizeH h1
  Nil          -> 1

main :: IO ()
main = do
  Lean.checkFor 1080 (prop_ToSortedList :: HeapPP Int -> Bool)
  check `for` 1080 $ (prop_ToSortedList :: HeapPP Int -> Bool)
