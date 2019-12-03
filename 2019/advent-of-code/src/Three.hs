module Three where

import Data.Map (Map)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

printSum :: IO ()
printSum = do
  lss  <- readInputs
  let a = fillValues (0,0) (lss !! 0)
  let b = Map.fromListWith (++) $ map (\(a1,a2) -> (a1,[a2])) $ fillValues (0,0) (lss !! 1)
  print $ findIntersection a b
  return ()

findIntersection :: [(Int,Int)] -> Map.Map Int [Int] -> Int
findIntersection x y = foldl min maxBound (map (\(a,b) -> (abs a) + (abs b)) $ filter filterFun x)
  where
    filterFun = (\(a1, a2) -> (justFun a1 a2) && (a1 /= 0 || a2 /= 0))
    justFun val val2 = case Map.lookup val y of
      Just ls -> val2 `elem` ls
      Nothing -> False

fillValues :: (Int,Int) -> [String] -> [(Int,Int)]
fillValues _ [] = []
fillValues curr (next:rest) = case next of
  ('R':val) -> let a = createTuple curr (read val) 0 in a ++ fillValues (last a) rest
  ('U':val) -> let a = createTuple curr 0 (read val) in a ++ fillValues (last a) rest
  ('L':val) -> let a = createTuple curr ((read val)*(-1)) 0 in a ++ fillValues (last a) rest
  ('D':val) -> let a = createTuple curr 0 ((read val)*(-1)) in a ++ fillValues (last a) rest

createTuple :: (Int,Int) -> Int -> Int -> [(Int,Int)]
createTuple (x0,y0) amountX amountY = [(x0 + x * absX,y0 + y * absY) | x <- [0..(abs amountX)], y <- [0..(abs amountY)]]
  where
    absX = if amountX == 0 then 1 else (abs amountX) `div` amountX
    absY = if amountY == 0 then 1 else (abs amountY) `div` amountY

readInputs :: IO [[String]]
readInputs = do
  text <- readFile "resources/input3.txt"
  return $ map (splitOn ",") $ lines text