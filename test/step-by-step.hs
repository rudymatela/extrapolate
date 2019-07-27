-- Copyright (c) 2017-2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (nub)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , tBackground prop
    == [ operatorE (is_ -==- is_)
       , operatorE (is_ -/=- is_)
       , operatorE (is_ -<=- is_)
       , operatorE (is_ -<-  is_)
       , lengthE
       , elemE

       , operatorE (b_ -==- b_)
       , operatorE (b_ -/=- b_)
       , notE

       , operatorE (i_ -==- i_)
       , operatorE (i_ -/=- i_)
       , operatorE (i_ -<=-  i_)
       , operatorE (i_ -<- i_)
       ]

  , concat (take 2 $ atoms prop)
    == [ b_
       , i_
       , is_
       , nil
       , false
       , true
       , zero

       , operatorE (is_ -==- is_)
       , operatorE (is_ -/=- is_)
       , operatorE (is_ -<=- is_)
       , operatorE (is_ -<-  is_)
       , lengthE
       , elemE

       , operatorE (b_ -==- b_)
       , operatorE (b_ -/=- b_)
       , notE

       , operatorE (i_ -==- i_)
       , operatorE (i_ -/=- i_)
       , operatorE (i_ -<=- i_)
       , operatorE (i_ -<-  i_)

       , val [0::Int]
       , one
       ]

  , snd thyes
    == [ b_
       , false
       , true
       , not' b_

       , is_  -==- is_
       , is_  -==- nil
       , is_  -/=- is_
       , is_  -/=- nil
       , is_  -<=- is_
       , is_  -<-  is_
       , elem' i_ is_
       , elem' zero is_

       , b_   -==- b_
       , b_   -/=- b_

       , i_   -==- i_
       , i_   -==- zero
       , i_   -/=- i_
       , i_   -/=- zero
       , i_   -<=- i_
       , i_   -<=- zero
       , zero -<=- i_
       , i_   -<-  i_
       , i_   -<-  zero
       , zero -<-  i_
       ]

  , candidateConditions (groundsFor prop) thyes (prop' xxs)
    == [ true
       , xxs -/=- nil
       , elem' zero xxs
       ]

  , validConditions thyes (groundsFor prop) (computeMinFailures prop) (prop' xxs)
    == [(false, 0)]

  , candidateConditions (groundsFor prop) thyes (prop' $ xx -:- xxs)
    == [ true
       , xxs -/=- nil
       , elem' xx xxs
       , elem' zero xxs
       , xx -/=- zero
       , xx -<=- zero
       , zero -<=- xx
       , xx -<- zero
       , zero -<- xx
       ]

  , validConditions thyes (groundsFor prop) (computeMinFailures prop) (prop' $ xx -:- xxs)
    =$ map fst
    $= [ (elem' xx xxs, 323) -- TODO: why is this 317 on GHC 8.0?
       , (false, 0)
       ]
  ]

thyes :: (Thy,[Expr])
thyes  =  theoryAndReprConds prop

prop' :: Expr -> Expr
prop' e  =  propE :$ e

propE :: Expr
propE  =  value "prop" prop_nubid

prop :: (WithOption (WithOption ([Int] -> Bool)))
prop   =  prop_nubid `With` MaxConditionSize 3 `With` ConstantBound Nothing

prop_nubid :: [Int] -> Bool
prop_nubid xs  =  nub xs == xs

elemE, lengthE :: Expr
lengthE = value "length" (length :: [Int] -> Int)
elemE = value "elem" (elem :: Int -> [Int] -> Bool)
