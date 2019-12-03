module Two
  (printSum
  ) where

import Data.List.Split

printSum :: IO ()
printSum = do
  (verb,noun) <- iterateOperations
  print $ 100 * verb + noun

iterateOperations :: IO (Int, Int)
iterateOperations = do
  program' <- readInputs
  let params = [(x,y) | x <- [0..99], y <- [0..99]]
  let program a b = replaceAtIndex 2 b (replaceAtIndex 1 a program')
  let calculate ((x,y):xs) = if performProgram (program x y) 0 == 19690720 then (x,y) else calculate xs
  return $ calculate params

readInputs :: IO [Int]
readInputs = do
    input <- readFile "resources/input2.txt"
    return $ map read $ splitOn "," input

performProgram :: [Int] -> Int -> Int
performProgram ls i
  | (ls !! i) == 99 = ls !! 0
  | otherwise = performProgram ls' (i + 4)
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

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b)
  where
    (a, (_:b)) = splitAt n ls