{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
import Test.Extrapolate
import Test.LeanCheck.Utils
import qualified Test.LeanCheck as Lean
import Data.Int

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable T
#endif

type I  =  [Int16]
data T  =  T I I I I I
  deriving Show

toList :: T -> [[Int16]]
toList (T i j k l m) = [i,j,k,l,m]

pre :: T -> Bool
pre t  =  all ((< 256) . sum) (toList t)

post :: T -> Bool
post t  =  (sum . concat) (toList t) < 5 * 256

prop :: T -> Bool
prop t  =  pre t ==> post t

instance Listable Int16 where
  list = map unX list

instance Generalizable Int16 where
  name _ = "x"
  expr = showConstant
  instances x = this x id
  background x = bgOrd x

{-
instance Listable T where
  tiers = cons5 makeT
    where
    makeT (Xs i) (Xs j) (Xs k) (Xs l) (Xs m) = T i j k l m
-}

deriveListable ''T
deriveGeneralizable ''T

main :: IO ()
main = do
  check `for` 10000
        `withConditionSize` 2
-- NOTE: it takes 25m to run with the following two lines uncommented
--      `withBackground` [constant "sum" (sum :: [Int16] -> Int16)]
--      `withConditionSize` 4
    $ prop
