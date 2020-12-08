import qualified Data.Map as Map
import Data.List.Split
import Data.List hiding (insert)

import Debug.Trace

main = two
----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput

  putStrLn $ show $ (findContainedBags input "shiny gold") - 1

findContainedBags :: BagMap -> Bag -> Int
findContainedBags mapp "no bag" = 0
findContainedBags mapp bag = count start
  where
    start = mapp Map.! bag
    count startLs = 1 + ( sum $ map countFun startLs)
    countFun (nextBag, count') = count' * (findContainedBags mapp nextBag)
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput

  putStrLn $ show $ length $ findEnclosingBagMap input "shiny gold"

findEnclosingBagMap :: BagMap -> Bag -> Bags
findEnclosingBagMap mapp bag = nub allOfThem
  where
    enclosing = Map.keys $ Map.filter (\ls -> any ((bag ==) . fst) ls) mapp
    allOfThem = enclosing ++ concatMap (findEnclosingBagMap mapp) enclosing
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO BagMap
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input


formatInput :: String -> BagMap
formatInput input = bagMap
  where
    bagMap = foldl (\currMap line -> Map.insert (index line) (contents line) currMap) Map.empty (lines input)
    index line = concat $ intersperse " " $ take 2 $ words line
    contents line = map contentFun $ chunksOf 4 $ drop 4 (words line)
    contentFun (count:c1:c2:a:xs) = (c1 ++ " " ++ c2, read count)
    contentFun (count:c1:c2:xs) = ("no bag", 1)

type BagMap = Map.Map Bag [(Bag, Int)]

type Bag = String
type Bags = [Bag]
