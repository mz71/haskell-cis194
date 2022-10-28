main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> print (n+1)))