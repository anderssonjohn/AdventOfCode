module One
  (printSum
  ) where

printSum :: IO ()
printSum = do
    inputs <- readInputs
    print $ sum $ map calculateFuel2 inputs


readInputs :: IO [Integer]
readInputs = do
    input <- readFile "resources/input1.txt"
    return $  map read $ lines input

calculateFuel :: Integer -> Integer
calculateFuel mass = mass `div` 3 - 2

calculateFuel2 :: Integer -> Integer
calculateFuel2 mass
  | fuel > 0 = fuel + calculateFuel2 fuel
  | otherwise = 0
    where
      fuel = calculateFuel mass
