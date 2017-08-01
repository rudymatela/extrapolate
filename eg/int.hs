import Test.Extrapolate

main :: IO ()
main = do
  putStrLn "Properties that should be OK:"
  check $ \x y -> x + y == y + (x :: Int)
  check $ \x y -> x * y == y * (x :: Int)
  check $ \x y z -> x + (y + z) == (x + y) + (z :: Int)
  check $ \x y z -> x * (y * z) == (x * y) * (z :: Int)
  putStrLn ""

  putStrLn "Properties that should fail:"
  check $ \x -> x + 1 == (x :: Int)
  check $ \x -> x /= (x :: Int)
  check $ \x y -> x /= (y :: Int)
  check $ \x y z -> x /= y && y /= (z :: Int)
  check $ \(x,y) -> x /= (y :: Int)
  check $ \(x,y,z) -> x /= y && y /= (z :: Int)
