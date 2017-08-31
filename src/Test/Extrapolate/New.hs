-- |
-- This module is otherwise unused in the code.
--
-- This is a stub of a new algorithm that is smarter and generalizes from
-- several initial counter-examples rather than just one.
--
-- When this gets finished, it should be moved into "Test.Extrapolate.Core".
module Test.Extrapolate.New
  ( generalizedCounterExamples
  , lgg
  , lgg1
  )
where

import Test.Extrapolate.Core


-- This is not the actual function used to generate generalizedCounterExamples.
-- It is otherwise unused elsewhere in the code.  It is a sketch of a new
-- version.  Please see counterExampleGens and generalizationsCE to see how it
-- works _now_.
generalizedCounterExamples :: Testable a => Int -> a -> [Exprs]
generalizedCounterExamples n p = gce $ counterExamples n p
  where
  passes = [as | (as,True) <- take n (results p)]
  gce :: [Exprs] -> [Exprs]
  gce []     = []
  gce (e:es) = foldr1 incorporate (e:es)
             : gce es
  incorporate :: Exprs -> Exprs -> Exprs
  g `incorporate` e = let g' = lgg g e
                      in if not $ any (`areInstancesOf` g') passes
                         then g'
                         else g


-- | Computes the least general generalization of two expressions
--
-- > lgg1 (expr [0,0]) (expr [1,1])
-- [_,_] :: [Int]  (holes: Int, Int)
-- > lgg1 (expr [1,1::Int]) (expr [2,2,2::Int])
-- _:_:_ :: [Int]  (holes: Int, Int, [Int])
lgg1 :: Expr -> Expr -> Expr
lgg1 e1 e2 | typ e1 /= typ e2  =
  error $ "lgg1: type mismatch: " ++ show e1 ++ ", " ++ show e2
lgg1 (e1f :$ e1x) (e2f :$ e2x)  |  typ e1f == typ e2f
                                && typ e1x == typ e2x
                                =  lgg1 e1f e2f :$ lgg1 e1x e2x
lgg1 e1@(Var _ _) _  =  e1
lgg1 _ e2@(Var _ _)  =  e2
lgg1 e1 e2 | e1 == e2   =  e1
           | otherwise  =  holeOfTy $ typ e1

lgg :: Exprs -> Exprs -> Exprs
lgg = zipWith lgg1
