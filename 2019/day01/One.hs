module Day01.One where

b :: IO ()
b = do
    inputs <- readInputs
    print $ sum $ map calculateFuel2 inputs

readInputs :: IO [Integer]
readInputs = do
    input <- readFile "input.txt"
    return $  map read $ lines input

calculateFuel :: Integer -> Integer
calculateFuel mass = mass `div` 3 - 2
a :: Float
calculateFuel2 :: Integer -> Integer
calculateFuel2 mass
  | fuel > 0 = fuel + calculateFuel2 fuel
  | otherwise = 0
    where
      fuel = calculateFuel mass

calculateFuel3 :: Integer -> Integer
calculateFuel3 i = power i 2


power :: Integer -> Integer -> Integer
power base exp = product $ replicate (fromInteger exp) base

