-- |
-- Module      : Test.Extrapolate.Basic
-- Copyright   : (c) 2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This provides the basic functionality of extrapolate.  You will have better
-- luck importing "Test.Extrapolate" directly.
module Test.Extrapolate.Basic
  ( module Test.Extrapolate.Core
  )
where

import Test.Extrapolate.Core
import Data.Ratio

instance (Integral a, Generalizable a) => Generalizable (Ratio a) where
  expr = showConstant
  name _ = "q"
  instances q = this q id
-- The following would allow zero denominators
-- expr (n % d) = constant "%" ((%) -:> n) :$ expr n :$ expr d

instance ( Generalizable a, Generalizable b, Generalizable c, Generalizable d
         , Generalizable e )
      => Generalizable (a,b,c,d,e) where
  name xyzwv = name ((\(x,_,_,_,_) -> x) xyzwv)
            ++ name ((\(_,y,_,_,_) -> y) xyzwv)
            ++ name ((\(_,_,z,_,_) -> z) xyzwv)
            ++ name ((\(_,_,_,w,_) -> w) xyzwv)
            ++ name ((\(_,_,_,_,v) -> v) xyzwv)
  expr (x,y,z,w,v) = constant ",,,," ((,,,,) ->>>>>: (x,y,z,w,v))
                  :$ expr x :$ expr y :$ expr z :$ expr w :$ expr v
  instances xyzwv = this xyzwv
                  $ instances ((\(x,_,_,_,_) -> x) xyzwv)
                  . instances ((\(_,y,_,_,_) -> y) xyzwv)
                  . instances ((\(_,_,z,_,_) -> z) xyzwv)
                  . instances ((\(_,_,_,w,_) -> w) xyzwv)
                  . instances ((\(_,_,_,_,v) -> v) xyzwv)
