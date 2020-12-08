import Data.List

----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let ids = map calculateSeatID input
  putStrLn $ show $ findOurId (sort ids)

findOurId :: [Int] -> Int
findOurId (x:xs) = if (x + 1) `elem` xs then findOurId xs else x + 1
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let ids = map calculateSeatID input
  putStrLn $ show $ maximum ids

calculateSeatID :: String -> Int
calculateSeatID id = rowID * 8 + columnID
  where
    (row, column) = splitAt 7 id
    rowID = id' row [0..127] 'F'
    columnID = id' column [0..7] 'L'
    id' [] (x:xs) _ = x
    id' (x:xs) ls char = let newLs = splitAt ((length ls) `div` 2) ls in
                         if x == char then id' xs (fst newLs) char
                         else id' xs (snd newLs) char
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = lines input

type Type = String
type TYPE = [Type]
