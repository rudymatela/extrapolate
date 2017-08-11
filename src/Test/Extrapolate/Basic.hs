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

instance ( Generalizable a, Generalizable b, Generalizable c, Generalizable d
         , Generalizable e, Generalizable f )
      => Generalizable (a,b,c,d,e,f) where
  name xyzwvu = name ((\(x,_,_,_,_,_) -> x) xyzwvu)
             ++ name ((\(_,y,_,_,_,_) -> y) xyzwvu)
             ++ name ((\(_,_,z,_,_,_) -> z) xyzwvu)
             ++ name ((\(_,_,_,w,_,_) -> w) xyzwvu)
             ++ name ((\(_,_,_,_,v,_) -> v) xyzwvu)
             ++ name ((\(_,_,_,_,_,u) -> u) xyzwvu)
  expr (x,y,z,w,v,u) = constant ",,,,," ((,,,,,) ->>>>>>: (x,y,z,w,v,u))
                    :$ expr x :$ expr y :$ expr z :$ expr w :$ expr v :$ expr u
  instances xyzwvu = this xyzwvu
                   $ instances ((\(x,_,_,_,_,_) -> x) xyzwvu)
                   . instances ((\(_,y,_,_,_,_) -> y) xyzwvu)
                   . instances ((\(_,_,z,_,_,_) -> z) xyzwvu)
                   . instances ((\(_,_,_,w,_,_) -> w) xyzwvu)
                   . instances ((\(_,_,_,_,v,_) -> v) xyzwvu)
                   . instances ((\(_,_,_,_,_,u) -> u) xyzwvu)
