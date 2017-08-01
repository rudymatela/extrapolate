-- |
-- Module      : Test.Extrapolate.IO
-- Copyright   : (c) 2017 Rudy Matela
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
  ( canonicalizeWith
  , grounds
  , groundsAndBinds
  , vassignments
  , vars
  , fold
  , unfold
  , isAssignmentTest

  , module Test.Speculate.Expr
  )
where

import Test.Speculate.Expr hiding
  ( ins
  , canonicalizeWith
  , grounds
  , vassignments
  , vars
  )
import qualified Test.Speculate.Expr as E
import qualified Test.Speculate.Engine as E
import Test.LeanCheck.Error (errorToFalse)
import Data.Typeable (typeOf, TypeRep)
import Data.List ((\\))

canonicalizeWith :: Instances -> [Expr] -> [Expr]
canonicalizeWith is = unfold . canonicalizeWith1 is . unrepeatedToHole1 . fold

canonicalizeWith1 :: Instances -> Expr -> Expr
canonicalizeWith1 ti e = e `assigning` ((\(t,n,n') -> (n,Var n' t)) `map` cr [] e)
  where
  cr :: [(TypeRep,String,String)] -> Expr -> [(TypeRep,String,String)]
  cr bs (e1 :$ e2) = cr (cr bs e1) e2
  cr bs (Var n t)
    | n == "" = bs
    | any (\(t',n',_) -> t == t' && n == n') bs = bs
    | otherwise = (t,n,head $ names ti t \\ map (\(_,_,n) -> n) bs):bs
  cr bs _ = bs

unrepeatedToHole1 :: Expr -> Expr
unrepeatedToHole1 e = e `assigning` [(n,Var "" t) | (t,n,1) <- countVars e]

grounds :: Instances -> [Expr] -> [ [Expr] ]
grounds is = map unfold . E.grounds is . fold

groundsAndBinds :: Instances -> [Expr] -> [(Binds,[Expr])]
groundsAndBinds is = map (mapSnd unfold) . E.groundAndBinds is . fold
  where
  mapSnd f (x,y) = (x,f y)

vassignments :: [Expr] -> [[Expr]]
vassignments = map unfold . E.vassignments . fold

vars :: [Expr] -> [(TypeRep,String)]
vars = E.vars . fold

isAssignmentTest :: Instances -> Int -> Expr -> Bool
isAssignmentTest is m e | typ e /= boolTy = False
isAssignmentTest is m e = length rs > 1 && length (filter id rs) == 1
  where
  rs = [errorToFalse $ eval False e' | [e'] <- take m $ grounds is [e]]

data MarkerType = MarkerType

fold :: [Expr] -> Expr
fold []     = constant "[]" MarkerType
fold (e:es) = constant ":"  MarkerType :$ e :$ fold es

unfold :: Expr -> [Expr]
unfold   e'@(Constant "[]" _)              | typ e' == typeOf MarkerType  =  []
unfold ((e'@(Constant ":"  _) :$ e) :$ es) | typ e' == typeOf MarkerType  =  e : unfold es
unfold e  =  error $ "unfold: cannot unfold expression: " ++ showPrecExpr 0 e
