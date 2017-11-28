-- Copyright (c) 2017 Colin Runciman and Rudy Matela Braquehais.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.TH
import Data.List
import Data.Char

prop_lengthWords0 :: String -> Bool
prop_lengthWords0 s  =  s /= ""
                    ==> length (words s) == length (filter isSpace s) + 1

prop_lengthWords1 :: String -> Bool
prop_lengthWords1 s  =  s /= "" && not (isSpace (head s))
                                && not (isSpace (last s))
                    ==> length (words s) == length (filter isSpace s) + 1

prop_lengthWords2 :: String -> Bool
prop_lengthWords2 s  =  noLeadingTrailingOrDoubleSpace s
                    ==> length (words s) == length (filter isSpace s) + 1
  where
  noLeadingTrailingOrDoubleSpace s = and [ not (isSpace a && isSpace b)
                                         | let s' = " " ++ s ++ " "
                                         , (a,b) <- zip s' (tail s') ]

main :: IO ()
main = do
  test prop_lengthWords0
--test prop_lengthWords1
