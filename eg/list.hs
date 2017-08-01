import Test.Extrapolate
import Data.List

main :: IO ()
main = do
  check $ \xs -> sort (sort xs :: [Int]) == sort xs
  check $ \xs ys -> xs `union` ys == ys `union` (xs :: [Int])
  check $ \xs -> reverse xs == (xs :: [Int])
  check `for` 1080 $ \xs -> nub xs == (xs :: [Int])
  check $ \xs -> null (xs :: [Int])
  check $ \xs xs' -> (xs++xs' :: [Int]) `isPrefixOf` xs
  check $ \xs xs' -> xs `isPrefixOf` (xs++xs' :: [Int])
