import Test.Extrapolate
import Test.LeanCheck.Utils
import qualified Test.LeanCheck as Lean

sort :: Ord a => [a] -> [a]
sort []      =  []
sort (x:xs)  =  sort (filter (< x) xs)
             ++ [x]
             ++ sort (filter (> x) xs)

prop_sortOrdered :: Ord a => [a] -> Bool
prop_sortOrdered xs  =  ordered (sort xs)
  where
  ordered (x:y:xs) = x <= y && ordered (y:xs)
  ordered _ = True

prop_sortCount :: Ord a => a -> [a] -> Bool
prop_sortCount x xs  =  count x (sort xs) == count x xs

count :: Eq a => a -> [a] -> Int
count x  =  length . filter (== x)

main :: IO ()
main  =  do
  chk $ prop_sortOrdered -:> [int]
  chk $ prop_sortCount   -:>  int
  check $ prop_sortCount -:>  int
  chk `for` 99 $ prop_sortOrdered -:> [bool]
  chk `for` 99 $ prop_sortCount   -:>  bool
  where
  chk :: Testable a => a -> IO ()
  chk = check `withBackground` [ value "count" $ count -:> int ]
              `withConditionSize` 6
