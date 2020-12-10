import Data.List

----------- Code for part two ------------------

two :: IO ()
two = do
  inp <- readInput
  let input = sort $ (0:inp) ++ [160]
  putStrLn $ show $ configurations input

configurations :: [Int] -> Integer
configurations = product . convertToPermutations . consecutiveOnes . diffs
  where
    convertToPermutations = map (toInteger . perms)
    perms val | val == 2 = 2
              | val == 3 = 4
              | val == 4 = 7
              | otherwise = val
    consecutiveOnes ls = map length $ filter (elem 1) $ group ls
    diffs (x:y:xs) = (y - x):(diffs (y:xs))
    diffs (x:xs) = [3]

----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let sorted = sort input
  let (one, three, _) = foldl (\(one, three,old) i -> if (i - old) == 1 then (one + 1, three, i) else (one, three + 1, i)) (0, 0,0) sorted
  putStrLn $ show (one * (three + 1))
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = map read $ lines input

type Type = Int
type TYPE = [Type]
