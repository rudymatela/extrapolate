{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
-- Example taken from Lee Pike's SmartCheck:
-- https://github.com/leepike/SmartCheck/blob/master/paper/paper.pdf
-- https://github.com/leepike/smartcheck
-- The version here is the one from the paper (similar to the one in the README
-- file).  I chose the one in the paper as it is clearer.
import Control.Monad
import Test.Extrapolate hiding (eval)
import Data.Maybe
import qualified Test.LeanCheck as Lean

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable Exp
#endif

data Exp = C Int
         | Add Exp Exp
         | Div Exp Exp
  deriving (Eq, Ord, Show)

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

instance Listable Exp where
  tiers  =  cons1 C
         \/ cons2 Add
         \/ cons2 Div

-- deriveGeneralizable ''Exp
-- {-
instance Generalizable Exp where
  expr (C i)        =  constant "C" C :$ expr i
  expr (Add e1 e2)  =  constant "Add" Add :$ expr e1 :$ expr e2
  expr (Div e1 e2)  =  constant "Div" Div :$ expr e1 :$ expr e2
  instances e   = these "e1" e
                [ constant "eval" eval
                , constant "divSubTerms" divSubTerms ]
                $ instances (undefined :: Int)
-- -}


main :: IO ()
main = do
  check prop_div
  check (isJust . eval)
  -- The following generalized counter-example from the paper is wrong!
  -- > forall x. Div x (Add (C (-5)) (C 5))
  print $ prop_div (Div (C 0) (C 0 `Add` C 0))
  -- setting x to (Div (C 0) (C 0)) makes the property pass
  print $ prop_div (Div (C 0 `Div` C 0) (C 0 `Add` C 0))
  -- As Lee Pike points out, SmartCheck's algorithm is unsound anyway,
  -- sometimes returning generalized counter-examples that are *too* general.
