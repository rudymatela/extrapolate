{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-} -- for GHC <= 7.8
import Test.Extrapolate
import Test.LeanCheck.Utils
import Data.Int

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable)
deriving instance Typeable T
#endif

type I  =  [Int8]
data T  =  T I I{- I-}
  deriving Show

toList :: T -> [[Int8]]
toList (T i j{- k-}) = [i,j{-,k-}]

pre :: T -> Bool
pre t  =  all ((< 16) . sum) (toList t)

post :: T -> Bool
post t  =  (sum . concat) (toList t) < 2 * 16 -- 3 * 16

prop :: T -> Bool
prop t  =  pre t ==> post t

instance Name Int8
instance Generalizable Int8 where
  background x = reifyEqOrd x

instance Listable T where
  tiers = cons2 makeT
    where
    makeT (Xs i) (Xs j) = T i j

deriveGeneralizable ''T

main :: IO ()
main = do
  check `for` 1080 -- 2160
        `withBackground` [value "sum" (sum :: [Int8] -> Int8)]
        `withConditionSize` 4
        $ prop
-- NOTE: with T defined as a triple, this takes 25s to run
