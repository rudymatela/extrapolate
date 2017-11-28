{-# LANGUAGE DeriveDataTypeable #-}
-- Copyright (c) 2017 Colin Runciman and Rudy Matela Braquehais.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.TH
import Data.Data
import Data.Typeable

import Data.List
import Data.Char

-- LazySmallCheck2012's Char enumeration does not contain spaces.  So we have
-- to create types to manually include them.

newtype CharS = CharS { unCharS :: Char }
  deriving (Eq, Ord, Data, Typeable)

newtype StringS = StringS { unStringS :: String }
  deriving (Eq, Ord, Data, Typeable)

instance Show CharS where
  show (CharS c) = show c

instance Show StringS where
  show (StringS s) = show s

instance Serial CharS where
  series = drawnFrom $ \d -> map CharS . take (d + 1) $ ' ':['a'..]

instance Serial StringS where
  series = (StringS . map unCharS) <$> series

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
  test (prop_lengthWords0 . unStringS)
--test (prop_lengthWords1 . unStringS)
--test prop_lengthWords0
--test prop_lengthWords1
