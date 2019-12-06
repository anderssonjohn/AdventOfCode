module Day06 where
import Data.List.Split
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

a :: IO ()
a = do
  input <- readInput "input.txt"
  calculateAllOrbits input

b :: IO ()
b = do
  input <- readInput "input.txt"
  calculateDistance input

test1 :: IO ()
test1 = do
  input <- readInput "testInput1.txt"
  calculateAllOrbits input

test2 :: IO ()
test2 = do
  input <- readInput "testInput2.txt"
  calculateDistance input


----------- Code for part b ------------------
calculateDistance :: [(String, [String])] -> IO ()
calculateDistance ls = do
  let mp = foldl insertMap Map.empty ls
  let youPath = findPath mp "YOU"
  let sanPath = findPath mp "SAN"
  let intersection = intersect youPath sanPath
  let result = (youPath \\ intersection) ++ (sanPath \\ intersection)
  print $ length result
  return ()

findPath :: (Map String [String]) -> String -> [String]
findPath mp node = case Map.lookup "COM" mp of
  Just ls -> findPath' mp "COM" node []
  Nothing -> [":("]

findPath' :: (Map String [String]) -> String -> String -> [String] -> [String]
findPath' mp from to curr = case Map.lookup from mp of
  Just ls -> if to `elem` ls then to:curr
    else curr ++ (concat (filter ((1 <) . length) (map (\s -> from:(findPath' mp s to curr)) ls)))
  Nothing -> []

----------- Code for part b ------------------

----------- Code for part a ------------------
calculateAllOrbits :: [(String, [String])] -> IO ()
calculateAllOrbits ls = do
  let mp = foldl insertMap Map.empty ls
  print $ calculateOrbits mp
  return ()

calculateOrbits :: (Map String [String]) -> Int
calculateOrbits mp = case Map.lookup "COM" mp of
  Just ls -> sum $ map (calculateOrbits' mp (1)) ls
  Nothing -> 2

calculateOrbits' :: (Map String [String]) -> Int -> String -> Int
calculateOrbits' mp val s = case Map.lookup s mp of
  Just ls -> val + (sum $ map (calculateOrbits' mp (val+1)) ls)
  Nothing -> val

insertMap :: (Map String [String]) -> (String, [String]) -> (Map String [String])
insertMap mp (a,b) = Map.insertWith (++) a b mp
----------- Code for part a ------------------

readInput :: String -> IO [(String, [String])]
readInput filePath = do
  input <- readFile filePath
  return $ map ((\[a,b] -> (a,[b])) . (splitOn ")")) $ lines input
