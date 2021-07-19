import Test.Extrapolate
import Data.Ratio

main :: IO ()
main = do
  -- correct properties
  check $ \q r -> q + r == r + (q :: Rational)
  check $ \q r -> q * r == r * (q :: Rational)
  check $ \q -> numerator q % denominator q == (q :: Rational)
  check $ \q -> numerator q /= 0
            ==> 1 / (1 / q) == (q :: Rational)

  -- incorrect properties
  check $ \q -> 1 / (1 / q) == (q :: Rational)
  check $ \q r -> denominator q == denominator r
              ==> denominator q == denominator (q + r :: Rational)
  check $ \x y d -> d > 0
                ==> denominator (x % d + y % d :: Rational) == d
