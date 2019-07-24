-- Copyright 2016--2017, Matt Might and Lee Pike.
--
-- Adapted from a SmartCheck example:
--   https://github.com/leepike/SmartCheck/tree/master/examples/RedBlackTreesh
-- which in turn was adapted from:
--   http://matt.might.net/articles/quick-quickcheck/
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

 -- Note from Matt Might:
 -- Note: Benjamin Pierce's lecture notes are where I learned to 
 -- generate properly ordered binary search trees:

 -- http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/BST.html

import RedBlackSet 

import Test.Extrapolate


instance (Ord a, Listable a) => Listable (RBSet a) where
  tiers = cons1 fromList

-- transforms a list into an RBSet.
fromList :: Ord a => [a] -> RBSet a
fromList = foldr insert empty

 -- Count the black depth of a red-black tree:
blackDepth :: RBSet a -> Maybe Int
blackDepth (E) = Just(1)
blackDepth (T R l _ r) = case (blackDepth(l),blackDepth(r)) of
  (Just(n),Just(m)) -> if n == m then Just(n) else Nothing
  (_,_) -> Nothing
blackDepth (T B l _ r) = case (blackDepth(l),blackDepth(r)) of
  (Just(n),Just(m)) -> if n == m then Just(1+n) else Nothing
  (_,_) -> Nothing
blackDepth _ = error "unhandled pattern"

 -- Check for red-red violations:
prop_NoRedRed :: RBSet Int -> Bool
prop_NoRedRed E = True
prop_NoRedRed (T R (T R _ _ _) _ _) = False
prop_NoRedRed (T R _ _ (T R _ _ _)) = False
prop_NoRedRed (T _ l x r) = (prop_NoRedRed l) && (prop_NoRedRed r)
prop_NoRedRed _ = error "unhandled pattern"


 -- Check for black-balanced violations:
prop_BlackBalanced :: RBSet Int -> Bool
prop_BlackBalanced t =
 case blackDepth(t) of
  Just _ -> True
  Nothing -> False


 -- Check for ordering violations:
prop_OrderedList :: Ord a => [a] -> Bool
prop_OrderedList [] = True
prop_OrderedList [x] = True
prop_OrderedList (x:y:tl) = (x < y) && (prop_OrderedList(y:tl))

prop_Ordered :: RBSet Int -> Bool
prop_Ordered t = prop_OrderedList (toAscList t) 

 -- Check for the validity of a red-black tree:
prop_RBValid :: RBSet Int -> Bool
prop_RBValid t = prop_NoRedRed t && prop_BlackBalanced t && prop_Ordered t


 -- Insertion properties:
prop_Create5 :: Int -> Int -> Int -> Int -> Int -> Bool
prop_Create5 a b c d e = 
  ((foldr insert empty) [a,b,c,d,e]) == 
  ((foldr insert empty) [b,c,d,e,a])

prop_InsertValid :: RBSet Int -> Int -> Bool
prop_InsertValid t x = prop_RBValid(insert x t)

prop_InsertMember :: RBSet Int -> Int -> Bool
prop_InsertMember t x = member x (insert x t)

prop_InsertSafe :: RBSet Int -> Int -> Int -> Bool
prop_InsertSafe t x y = member x t ==> (member x (insert y t))

prop_NoInsertPhantom :: RBSet Int -> Int -> Int -> Bool
prop_NoInsertPhantom t x y = 
 not (member x t) && x /= y ==> not (member x (insert y t))

 -- Deletion properties:
prop_InsertDeleteValid :: RBSet Int -> Int -> Bool
prop_InsertDeleteValid t x = prop_RBValid(delete x (insert x t))

prop_DeleteValid :: RBSet Int -> Int -> Bool
prop_DeleteValid t x = prop_RBValid(delete x t)

prop_MemberDelete :: RBSet Int -> Int -> Bool
prop_MemberDelete t x = member x t ==> not (member x (delete x t))

prop_DeletePreserve :: RBSet Int -> Int -> Int -> Bool
prop_DeletePreserve t x y = x /= y ==> (member y t) == (member y (delete x t))

main :: IO ()
main = do
  chk prop_BlackBalanced

  -- {-
  -- Insertion tests:
  chk prop_Create5
  chk prop_InsertValid
  chk prop_InsertSafe
  chk prop_NoInsertPhantom
  chk prop_InsertMember

  -- Deletion tests:
  chk prop_InsertDeleteValid
  chk prop_DeleteValid
  chk prop_MemberDelete
  chk prop_DeletePreserve
  -- -}
  where
  chk :: Testable a => a -> IO ()
  chk = check `for` 1080

instance Listable Color where
  tiers  =  [[R, B], [BB, NB]]

instance Name (RBSet a) where
  name _ = "h"

instance (Ord a, Express a) => Express (RBSet a) where
  expr t = value "fromList" (fromList ->: t) :$ expr (toAscList t)

instance (Ord a, Generalizable a) => Generalizable (RBSet a) where
  subInstances h  =  instances (toAscList h)

-- TODO: make LeanCheck find a counter-example to prop_BlackBalanced
--       when -DBALANCE_BUG is active or find out why it cannot find it.

-- TODO: somehow make "./eg/redblack-remove-bug" run faster.
--       (currently, it takes 2 minutes to run)
