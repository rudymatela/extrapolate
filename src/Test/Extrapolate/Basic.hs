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
