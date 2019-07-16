-- Refinements of a property about Data.List.words
--
-- Copyright (c) 2017 Colin Runciman and Rudy Matela Braquehais.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.Extrapolate
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
prop_lengthWords2 s  =  noDoubleSpace (" " ++ s ++ " ")
                    ==> length (words s) == length (filter isSpace s) + 1
  where
  noDoubleSpace s = and [ not (isSpace a && isSpace b)
                        | (a,b) <- zip s (tail s) ]

main :: IO ()
main = do
  check  prop_lengthWords0
  let check1 = check `withBackground` [value "isSpace" isSpace]
  check1 prop_lengthWords0
  check1 prop_lengthWords1
  let check2 = check `withBackground` [ value "`isInfixOf`" $ isInfixOf -:> string
                                      , val "  " ]
  check2 prop_lengthWords0 -- a step back, note the *zero*
  check  prop_lengthWords2
