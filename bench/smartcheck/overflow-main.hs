-- main for paperExample1

instance SubTypes I
instance SubTypes T

main :: IO ()
main = do
  smartCheck scStdArgs{format = PrintString} prop
