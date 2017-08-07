import Test.Extrapolate
import Data.List

main :: IO ()
main = do
  ch ()
  ch bool
  ch int
  ch integer
  ch char

  ch [()]
  ch [bool]
  ch [int]
  ch [integer]
  ch [char]

  ch ((),int)
  ch (bool,char)

  ch (int,(),bool)

  ch (mayb ())
  ch (mayb int)

  ch (integer,mayb (int,(),bool),mayb (char,()))

ch :: (Eq a, Generalizable a) => a -> IO ()
ch x = do
  check $ const True  -:> x
  check $ const False -:> x
  check $ (==) -:> x
  check $ (/=) -:> x
-- The following makes everything too slow
--check $ (\x y z -> x == y && y == z) -:> x
--check $ (\x y z -> x /= y && y /= z && z /= x) -:> x
--check $ (\x y z w -> x == y || z == w) -:> x ->>:> x
-- maybe in a future faster version we can uncomment those above.
