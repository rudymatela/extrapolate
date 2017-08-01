-- Example taken from Lee Pike's SmartCheck:
-- https://github.com/leepike/SmartCheck/blob/master/paper/paper.pdf
-- https://github.com/leepike/smartcheck
-- The version here is the one from the paper (similar to the one in the README
-- file).  I chose the one in the paper as it is clearer.
{-# Language TemplateHaskell, DeriveDataTypeable #-}
import Control.Monad
import Data.Maybe
import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.TH
import Data.Typeable
import Data.Data

data Exp = C Int
         | Add Exp Exp
         | Div Exp Exp
  deriving (Eq, Ord, Show, Data, Typeable)

eval :: Exp -> Maybe Int
eval (C i) = Just i
eval (Add e0 e1) =
  liftM2 (+) (eval e0) (eval e1)
eval (Div e0 e1) =
  let e = eval e1 in
  if e == Just 0 then Nothing
    else liftM2 div (eval e0) e

divSubTerms :: Exp -> Bool
divSubTerms (C _)         = True
divSubTerms (Div _ (C 0)) = False
divSubTerms (Add e0 e1)   =  divSubTerms e0
                          && divSubTerms e1
divSubTerms (Div e0 e1)   =  divSubTerms e0
                          && divSubTerms e1

prop_div :: Exp -> Bool
prop_div e = divSubTerms e ==> eval e /= Nothing

deriveSerial ''Exp

main :: IO ()
main = do
  test prop_div
