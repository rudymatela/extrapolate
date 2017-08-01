import Test.SmartCheck
import Test.QuickCheck
import Data.List

main :: IO ()
main = do
  quickCheck $ \xs -> nub xs == (xs :: [Int])
  smartCheck scStdArgs{format = PrintString} $ \xs -> nub xs == (xs :: [Int])
