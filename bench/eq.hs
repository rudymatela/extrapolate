import Test.Extrapolate
import Data.Typeable (typeOf)

main :: IO ()
main = do
  ch ()
  ch bool
  ch int
  ch integer
  ch char
  ch ordering

  ch [()]
  ch [bool]
  ch [int]
  ch [integer]
  ch [char]
  ch [ordering]

  ch ((),int)
  ch (bool,char)
  ch (int,(),bool)
  ch (ordering,char,integer)

  ch (mayb ())
  ch (mayb int)

  ch (eith () bool)
  ch (eith int char)

-- TODO: find out why output for the following is different across diff GHCs
-- see: https://travis-ci.org/rudymatela/extrapolate/jobs/263425540
-- ch (char,int,bool,ordering)

-- For some reason, output for the following funky types is different across
-- different versions of GHC, see:
-- https://travis-ci.org/rudymatela/extrapolate/builds/262325825
-- I don't have time to investigate now, but it might be good to check why.
--ch (eith integer ordering,mayb (int,(),bool),mayb (char,()))
--ch (eith (integer,mayb (int,(),bool),mayb (char,())) (int,char))


ch :: (Eq a, Generalizable a) => a -> IO ()
ch x = do
  putStrLn $ "checks :: " ++ show (typeOf x) ++ "\n"
  check $ (==) -:> x
  putStrLn ""
-- The following makes everything too slow
--check $ (\x y z -> x == y && y == z) -:> x
--check $ (\x y z w -> x == y || z == w) -:> x ->>:> x
-- maybe in a future faster version we can uncomment those above.
