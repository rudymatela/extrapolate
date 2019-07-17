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
    == [ operatorE (_is -==- _is)
       , operatorE (_is -/=- _is)
       , operatorE (_is -<=- _is)
       , operatorE (_is -<-  _is)
       , lengthE
       , elemE

       , operatorE (_b -==- _b)
       , operatorE (_b -/=- _b)
       , notE

       , operatorE (_i -==- _i)
       , operatorE (_i -/=- _i)
       , operatorE (_i -<-  _i)
       , operatorE (_i -<=- _i) ]

  , concat (take 2 $ atoms prop)
    == [ _b
       , _i
       , _is
       , ll
       , false
       , true
       , zero

       , operatorE (_is -==- _is)
       , operatorE (_is -/=- _is)
       , operatorE (_is -<=- _is)
       , operatorE (_is -<-  _is)
       , lengthE
       , elemE

       , operatorE (_b -==- _b)
       , operatorE (_b -/=- _b)
       , notE

       , operatorE (_i -==- _i)
       , operatorE (_i -/=- _i)
       , operatorE (_i -<-  _i)
       , operatorE (_i -<=- _i)

       , val [0::Int]
       , one
       ]

  , snd thyes
    == [ _b
       , false
       , true
       , not' _b

       , _is  -==- _is
       , _is  -==- ll
       , _is  -/=- _is
       , _is  -/=- ll
       , _is  -<=- _is
       , _is  -<-  _is
       , elem' _i _is
       , elem' zero _is

       , _b   -==- _b
       , _b   -/=- _b

       , _i   -==- _i
       , _i   -==- zero
       , _i   -/=- _i
       , _i   -/=- zero
       , _i   -<-  _i
       , _i   -<-  zero
       , zero -<-  _i
       , _i   -<=- _i
       , _i   -<=- zero
       , zero -<=- _i
       ]

  , candidateConditions thyes prop [xxs]
    == [ true
       , xxs -/=- ll
       , elem' zero xxs
       ]

  , validConditions thyes prop [xxs]
    == [(false, 0)]

  , candidateConditions thyes prop [xx -:- xxs]
    == [ true
       , xxs -/=- ll
       , elem' xx xxs
       , elem' zero xxs
       , xx -/=- zero
       , xx -<- zero
       , zero -<- xx
       , xx -<=- zero
       , zero -<=- xx
       ]

  , validConditions thyes prop [xx -:- xxs]
    =$ map fst
    $= [ (elem' xx xxs, 323) -- TODO: why is this 317 on GHC 8.0?
       , (false, 0)
       ]
  ]

thyes :: (Thy,[Expr])
thyes  =  theoryAndReprConds prop

prop :: (WithOption (WithOption ([Int] -> Bool)))
prop   =  prop_nubid `With` MaxConditionSize 3 `With` ConstantBound Nothing

prop_nubid :: [Int] -> Bool
prop_nubid xs  =  nub xs == xs

elemE, lengthE :: Expr
lengthE = value "length" (length :: [Int] -> Int)
elemE = value "elem" (elem :: Int -> [Int] -> Bool)
