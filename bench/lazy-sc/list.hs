import Test.LazySmallCheck2012
import Data.List

main :: IO ()
main = do
  test $ \xs xs' -> (xs++xs' :: [Int]) `isPrefixOf` xs

-- not lazy, so no generalization with Lazy SmallCheck 2012
-- > test $ \xs -> nub xs == (xs :: [Int])

-- not lazy as well:
-- > test $ \xs ys -> xs `union` ys == ys `union` (xs :: [Int])
