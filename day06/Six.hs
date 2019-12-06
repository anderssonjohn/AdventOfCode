module Day06 where
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

a :: IO ()
a = do
  input <- readInput "input.txt"
  main input

test1 :: IO ()
test1 = do
  input <- readInput "testInput1.txt"
  main input

main :: [(String, [String])] -> IO ()
main ls = do
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

readInput :: String -> IO [(String, [String])]
readInput filePath = do
  input <- readFile filePath
  return $ map ((\[a,b] -> (a,[b])) . (splitOn ")")) $ lines input
