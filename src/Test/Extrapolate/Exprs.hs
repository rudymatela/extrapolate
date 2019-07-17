{-# LANGUAGE DeriveDataTypeable #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Extrapolate.IO
-- Copyright   : (c) 2017-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Extrapolate,
-- a library for generalization of counter-examples.
--
-- This module re-exports some functionality from Test.Speculate.Expr, but
-- instead of working on single expressions it works in lists of expressions
-- (the choosen representation for counter-examples).
module Test.Extrapolate.Exprs
  ( Exprs
  , canonicalizeWith
  , grounds
  , groundsAndBinds
  , vassignments
  , vars
  , fold
  , unfold
  , isAssignmentTest
  , nameWith

  , module Test.Speculate.Expr
  )
where

import Test.Speculate.Expr hiding
  ( ins
  , canonicalizeWith
  , grounds
  , canonicalVariations
  , vars
  , nubVars
  , mkNameWith
  )
import qualified Test.Speculate.Expr as E
import qualified Test.Speculate.Engine as E
import Test.LeanCheck.Error (errorToFalse)
import Data.Typeable (typeOf, TypeRep, Typeable)
import Data.List ((\\))

type Exprs = [Expr]

nameWith :: Typeable a => String -> a -> Instances
nameWith = E.mkNameWith

canonicalizeWith :: Instances -> [Expr] -> [Expr]
canonicalizeWith is  =  unfold . c1 . unrepeatedToHole1 . fold
  where
  c1 e  =  e //- cn e
  cn e  =  E.canonicalizationWith (lookupNames is)
        $  fold [v | v <- E.vars e, not $ isHole v]

unrepeatedToHole1 :: Expr -> Expr
unrepeatedToHole1 e = e //- [(v, holeAsTypeOf v) | (v,1) <- countVars e]
  where
  countVars e = map (\e' -> (e',length . filter (== e') $ E.vars e)) $ E.nubVars e

grounds :: Instances -> [Expr] -> [ [Expr] ]
grounds is = map unfold . E.grounds is . fold

groundsAndBinds :: Instances -> [Expr] -> [(Binds,[Expr])]
groundsAndBinds is = map (mapSnd unfold) . E.groundAndBinds is . fold
  where
  mapSnd f (x,y) = (x,f y)

vassignments :: [Expr] -> [[Expr]]
vassignments = map unfold . E.canonicalVariations . fold

vars :: [Expr] -> [Expr]
vars = E.nubVars . fold

isAssignmentTest :: Instances -> Int -> Expr -> Bool
isAssignmentTest is m e | typ e /= boolTy = False
isAssignmentTest is m e = length rs > 1 && length (filter id rs) == 1
  where
  rs = [errorToFalse $ eval False e' | [e'] <- take m $ grounds is [e]]
