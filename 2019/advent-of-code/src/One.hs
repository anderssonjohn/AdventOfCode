module One
  (printSum
  ) where

printSum :: IO ()
printSum = do
    inputs <- readInputs
    print $ sum $ map calculateFuel inputs


readInputs :: IO [Integer]
readInputs = do
    input <- readFile "resources/input1.txt"
    return $  map read $ lines input

calculateFuel :: Integer -> Integer
calculateFuel mass = mass `div` 3 - 2
