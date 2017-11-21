-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (nub)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , snd thyes
    == [ _b
       , false
       , true
       , not' _b
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
       , _b   -==- _b
       , _b   -/=- _b
       , elem' _i _is
       , elem' zero _is
       , _is  -==- _is
       , _is  -==- ll
       , _is  -/=- _is
       , _is  -/=- ll
       , _is  -<=- _is
       , _is  -<-  _is
       ]

  , candidateConditions thyes prop [xxs]
    == [ false
       , true
       , elem' zero xxs
       , xxs -/=- ll
       ]

  , validConditions thyes prop [xxs]
    == [(false, 0)]

  , candidateConditions thyes prop [xx -:- xxs]
    == [ false
       , true
       , xx -/=- zero
       , xx -<- zero
       , zero -<- xx
       , xx -<=- zero
       , zero -<=- xx
       , elem' xx xxs
       , elem' zero xxs
       ,  xxs -/=- ll
       ]

  , validConditions thyes prop [xx -:- xxs]
    == [ (elem' xx xxs, 323)
       , (false, 0)
       ]
  ]

thyes  =  theoryAndReprConds prop
prop   =  prop_nubid `With` MaxConditionSize 3 `With` ConstantBound Nothing

prop_nubid :: [Int] -> Bool
prop_nubid xs  =  nub xs == xs
