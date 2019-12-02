module Two
  (printSum
  ) where

import Data.List.Split

printSum :: IO ()
printSum = do
  program <- readInputs
  print $ performProgram program 0

readInputs :: IO [Int]
readInputs = do
    input <- readFile "resources/input2.txt"
    return $ map read $ splitOn "," input

performProgram :: [Int] -> Int -> Int
performProgram ls i
  | (ls !! i) == 99 = ls !! 0
  | otherwise = performProgram ls' (i + 4) -- @(op:pos1:pos2:target:rest) =
  where
    val1 = ls !! (ls !! (i + 1))
    val2 = ls !! (ls !! (i + 2))
    result = calculate (ls !! i) val1 val2
    ls' = replaceAtIndex (ls !! (i + 3)) result ls

calculate :: Int -> Int -> Int -> Int
calculate 1 val1 val2 = val1 + val2
calculate 2 val1 val2 = val1 * val2
calculate 99 _ _ = -1
calculate _ _ _ = error "Invalid operation code"

replaceAtIndex n item ls = a ++ (item:b)
  where
    (a, (_:b)) = splitAt n ls