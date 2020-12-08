import Data.List
import Data.Set (fromList, intersection, empty, size)
----------- Code for part two ------------------
mainTwo :: IO ()
mainTwo = do
  input <- readInput
  let sets = map (map fromList) input
  let setInter = map (foldl intersection (fromList ['a'..'z'])) sets
  let count = foldl (\curr str -> curr + (size str)) 0 setInter
  putStrLn $ show $ count
----------- Code for part two ------------------
----------- Code for part one ------------------
mainOne :: IO ()
mainOne = do
  input <- readInput
  let uniqueStrings = map nub $ map concat $ input
  let count = foldl (\curr str -> curr + (length str)) 0 uniqueStrings
  putStrLn $ show $ count
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = removeEmpty $ groupBy (\a b -> b /= "") $ lines input
  where
    removeEmpty inp = map (filter (not . null)) inp

type Type = [String]
type TYPE = [Type]
