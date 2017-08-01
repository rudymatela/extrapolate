{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.TH
import Data.Int
import Data.Data
import Data.Typeable

type I  =  [Int16]
data T  =  T I I I I I
  deriving (Show, Data, Typeable)

toList :: T -> [[Int16]]
toList (T i j k l m) = [i,j,k,l,m]

pre :: T -> Bool
pre t  =  all ((< 256) . sum) (toList t)

post :: T -> Bool
post t  =  (sum . concat) (toList t) < 5 * 256

prop :: T -> Bool
prop t  =  pre t ==> post t

instance Serial Int16 where
  series = drawnFrom $ \d -> map fromIntegral [(-d)..d]

deriveSerial ''T

main :: IO ()
main = test prop
