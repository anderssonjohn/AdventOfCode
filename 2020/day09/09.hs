import Data.Set hiding (map, drop, take, filter)
import qualified Data.Set as Set (map, null, foldl, filter)
main = two
----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let result = findRange 0 2 input
  putStrLn $ show $ (minimum result) + (maximum result)


findRange :: Int -> Int -> [Int] -> [Int]
findRange start stop ls = if currSum == 507622668 then currLs else
  if currSum < 507622668 then
    findRange start (stop + 1) ls
  else
    findRange (start + 1) stop ls
  where
    currLs = take (stop - start) $ drop start ls
    currSum = sum currLs
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let sets = [(fromList (take 25 (drop x input)), head (drop (x + 25) input)) | x <- [0..(length input - 26)]]
  let result = filter (-1 /=) $ map isValid sets
  putStrLn $ show result


isValid :: (Set Int, Int) -> Int
isValid (set, int) = if contains set int then -1 else int

contains :: Set Int -> Int -> Bool
contains set val = or $ Set.map (\num -> member (val - num) set) set
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
