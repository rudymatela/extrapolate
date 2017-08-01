-- |
-- Module      : Test.Extrapolate
-- Copyright   : (c) 2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Extrapolate is a library for generalization of counter-examples.
--
-- Consider the following faulty implementation of sort:
--
-- > sort :: Ord a => [a] -> [a]
-- > sort [] = []
-- > sort (x:xs) = sort (filter (< x) xs)
-- >            ++ [x]
-- >            ++ sort (filter (> x) xs)
--
-- Extrapolate works like so:
--
-- > > check $ \xs -> ordered (sort xs)
-- > +++ OK, passed 360 tests!
-- > > check $ \x xs -> count x (sort xs) == count x xs
-- > *** Failed! Falsifiable (after 4 tests):
-- > 0 [0,0]
-- > Generalization:
-- > x (x:x:xs)
module Test.Extrapolate
  ( module Test.Extrapolate.Core
  , module Test.Extrapolate.Basic
  , module Test.Extrapolate.Derive
  , module Test.Extrapolate.TypeBinding
  , module Test.Extrapolate.IO
  )
where

import Test.Extrapolate.Core
import Test.Extrapolate.Basic
import Test.Extrapolate.Derive
import Test.Extrapolate.TypeBinding
import Test.Extrapolate.IO
