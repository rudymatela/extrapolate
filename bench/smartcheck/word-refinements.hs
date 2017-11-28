-- Copyright (c) 2017 Colin Runciman and Rudy Matela Braquehais.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.SmartCheck
import Test.QuickCheck
import Data.List
import Data.Char

prop_lengthWords0 :: String -> Property
prop_lengthWords0 s  =  s /= ""
                    ==> length (words s) == length (filter isSpace s) + 1

prop_lengthWords1 :: String -> Property
prop_lengthWords1 s  =  s /= "" && not (isSpace (head s))
                                && not (isSpace (last s))
                    ==> length (words s) == length (filter isSpace s) + 1

prop_lengthWords2 :: String -> Property
prop_lengthWords2 s  =  noLeadingTrailingOrDoubleSpace s
                    ==> length (words s) == length (filter isSpace s) + 1
  where
  noLeadingTrailingOrDoubleSpace s = and [ not (isSpace a && isSpace b)
                                         | let s' = " " ++ s ++ " "
                                         , (a,b) <- zip s' (tail s') ]

main :: IO ()
main = do
  smartCheck scStdArgs{format = PrintString} prop_lengthWords0
--smartCheck scStdArgs{format = PrintString} prop_lengthWords1
