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

type I  =  [Int8]
data T  =  T I I I
  deriving Show

toList :: T -> [[Int8]]
toList (T i j k) = [i,j,k]

pre :: T -> Bool
pre t  =  all ((< 16) . sum) (toList t)

post :: T -> Bool
post t  =  (sum . concat) (toList t) < 5 * 16

prop :: T -> Bool
prop t  =  pre t ==> post t

instance Listable Int8 where
  list = map unX list

instance Generalizable Int8 where
  name _ = "x"
  expr = showConstant
  instances x = this x id
--background x = bgOrd x -- TODO: make this work (performance issue)

deriveListable ''T
deriveGeneralizable ''T

main :: IO ()
main = do
  check `for` 2500 $ prop
--print $ take 1000 (list :: [T])
