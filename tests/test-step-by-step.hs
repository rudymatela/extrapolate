-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , representativeConditions (([int] >- bool) `With` MaxConditionSize 3)
    == [ true
       , _b
       , not' _b
       , zero -==- _i
       , _i   -==- _i
       , zero -/=- _i
       , _i   -/=- _i
       , zero -<-  _i
       , _i   -<-  zero
       , _i   -<-  _i
       , zero -<=- _i
       , _i   -<=- zero
       , _i   -<=- _i
       , _b   -==- _b
       , _b   -/=- _b
       , elem' zero _is
       , elem' _i _is
       , ll   -==- _is
       , _is  -==- _is
       , ll   -/=- _is
       , _is  -/=- _is
       , _is  -<=- _is
       , _is  -<-  _is
       ]
  ]

representativeConditions :: Testable a => a -> [Expr]
representativeConditions = snd . theoryAndReprConds
