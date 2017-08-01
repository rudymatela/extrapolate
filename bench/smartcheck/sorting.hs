import Test.SmartCheck
import System.Environment (getArgs)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort (filter (< x) xs)
           ++ [x]
           ++ sort (filter (> x) xs)

prop_sortOrdered :: Ord a => [a] -> Bool
prop_sortOrdered xs = ordered (sort xs)
  where
  ordered (x:y:xs) = x <= y && ordered (y:xs)
  ordered _ = True

prop_sortCount :: Ord a => a -> [a] -> Bool
prop_sortCount x xs = count x (sort xs) == count x xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

main :: IO ()
main = do
  as <- getArgs
  case as of
    ("uncurry":_) -> smartCheck scStdArgs { format = PrintString
                                          , runExists = False }
                    (uncurry prop_sortCount :: (Int,[Int]) -> Bool)
    ("swap":_) -> smartCheck scStdArgs { format = PrintString
                                       , runExists = False }
                    (flip prop_sortCount :: [Int] -> Int -> Bool)
    _          -> smartCheck scStdArgs { format = PrintString
                                       , runExists = False }
                    (prop_sortCount :: Int -> [Int] -> Bool)
