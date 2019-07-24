{-# Language TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
-- Example taken from Lee Pike's SmartCheck:
-- https://github.com/leepike/SmartCheck/blob/master/paper/paper.pdf
-- https://github.com/leepike/smartcheck
-- The version here is the one from the paper (similar to the one in the README
-- file).  I chose the one in the paper as it is clearer.
import Control.Monad
import Test.Extrapolate hiding (eval)
import Data.Maybe


#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable Exp
#endif

data Exp a = C a
           | Add (Exp a) (Exp a)
           | Div (Exp a) (Exp a)
  deriving (Eq, Ord, Show)

eval :: Integral a => Exp a -> Maybe a
eval (C i) = Just i
eval (Add e0 e1) =
  liftM2 (+) (eval e0) (eval e1)
eval (Div e0 e1) =
  let e = eval e1 in
  if e == Just 0 then Nothing
    else liftM2 div (eval e0) e

-- originally called divSubTerms by Pike
noDiv0 :: Integral a => Exp a -> Bool
noDiv0 (C _)         = True
noDiv0 (Div _ (C 0)) = False
noDiv0 (Add e0 e1)   =  noDiv0 e0 && noDiv0 e1
noDiv0 (Div e0 e1)   =  noDiv0 e0 && noDiv0 e1

prop_div :: Integral a => Exp a -> Bool
prop_div e = noDiv0 e ==> eval e /= Nothing

instance Listable a => Listable (Exp a) where
  tiers  =  cons1 C
         \/ cons2 Add
         \/ cons2 Div

-- deriveGeneralizable ''Exp
-- {-
instance Name a => Name (Exp a) where  name _  =  "e1"

instance Express a => Express (Exp a) where
  expr e@(C i)        =  value "C"   (C    ->: e) :$ expr i
  expr e@(Add e1 e2)  =  value "Add" (Add ->>: e) :$ expr e1 :$ expr e2
  expr e@(Div e1 e2)  =  value "Div" (Div ->>: e) :$ expr e1 :$ expr e2

instance Generalizable a => Generalizable (Exp a) where
  subInstances e  =  instances i
                     where C i = e
-- -}

argTypeOf :: (a -> b) -> a -> (a -> b)
argTypeOf f x = f

main :: IO ()
main = do
  check (prop_div :: Exp Int -> Bool)
  check ((isJust . eval) :: Exp Int -> Bool)
  -- The following generalized counter-example from the paper is wrong!
  -- > forall x. Div x (Add (C (-5)) (C 5))
  print $ prop_div (Div (C 0) (C 0 `Add` C 0))
  -- setting x to (Div (C 0) (C 0)) makes the property pass
  print $ prop_div (Div (C 0 `Div` C 0) (C 0 `Add` C 0))
  -- As Lee Pike points out, SmartCheck's algorithm is unsound anyway,
  -- sometimes returning generalized counter-examples that are *too* general.
