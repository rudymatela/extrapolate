{-# LANGUAGE CPP #-} -- for GHC <= 7.8
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
  , module Test.Extrapolate.New
  )
where

import Test.Extrapolate.Core
import Test.Extrapolate.New
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
  name xyzwv = name x ++ name y ++ name z ++ name w ++ name v
               where (x,y,z,w,v) = xyzwv
  expr (x,y,z,w,v) = constant ",,,," ((,,,,) ->>>>>: (x,y,z,w,v))
                  :$ expr x :$ expr y :$ expr z :$ expr w :$ expr v
  instances xyzwv = this xyzwv $ instances x . instances y . instances z
                               . instances w . instances v
                    where (x,y,z,w,v) = xyzwv

instance ( Generalizable a, Generalizable b, Generalizable c, Generalizable d
         , Generalizable e, Generalizable f )
      => Generalizable (a,b,c,d,e,f) where
  name xyzwvu = name x ++ name y ++ name z ++ name w ++ name v ++ name u
          where (x,y,z,w,v,u) = xyzwvu
  expr (x,y,z,w,v,u) = constant ",,,,," ((,,,,,) ->>>>>>: (x,y,z,w,v,u))
                    :$ expr x :$ expr y :$ expr z :$ expr w :$ expr v :$ expr u
  instances xyzwvu = this xyzwvu $ instances x . instances y . instances z
                                 . instances w . instances v . instances u
               where (x,y,z,w,v,u) = xyzwvu

instance ( Generalizable a, Generalizable b, Generalizable c, Generalizable d
         , Generalizable e, Generalizable f, Generalizable g )
      => Generalizable (a,b,c,d,e,f,g) where
  name xyzwvut = name x ++ name y ++ name z ++ name w
              ++ name v ++ name u ++ name t
           where (x,y,z,w,v,u,t) = xyzwvut
  expr (x,y,z,w,v,u,t) = constant ",,,,,," ((,,,,,,) ->>>>>>>: (x,y,z,w,v,u,t))
                      :$ expr x :$ expr y :$ expr z :$ expr w
                      :$ expr v :$ expr u :$ expr t
  instances xyzwvut = this xyzwvut
                    $ instances x . instances y . instances z . instances w
                    . instances v . instances u . instances t
                where (x,y,z,w,v,u,t) = xyzwvut

#if __GLASGOW_HASKELL__ < 710
-- No 8-tuples for you:
-- On GHC 7.8, 8-tuples are not Typeable instances.  We could add a standalone
-- deriving clause, but that may cause trouble if some other library does the
-- same.  User should declare Generalizable 8-tuples manually when using GHC <=
-- 7.8.
#else
instance ( Generalizable a, Generalizable b, Generalizable c, Generalizable d
         , Generalizable e, Generalizable f, Generalizable g, Generalizable h )
      => Generalizable (a,b,c,d,e,f,g,h) where
  name xyzwvuts = name x ++ name y ++ name z ++ name w
               ++ name v ++ name u ++ name t ++ name s
    where (x,y,z,w,v,u,t,s) = xyzwvuts
  expr (x,y,z,w,v,u,t,s) = constant ",,,,,,," ((,,,,,,,) ->>>>>>>>: (x,y,z,w,v,u,t,s))
                        :$ expr x :$ expr y :$ expr z :$ expr w
                        :$ expr v :$ expr u :$ expr t :$ expr s
  instances xyzwvuts = this xyzwvuts
                     $ instances x . instances y . instances z . instances w
                     . instances v . instances u . instances t . instances s
    where (x,y,z,w,v,u,t,s) = xyzwvuts
#endif
