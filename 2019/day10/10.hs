import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Pos = (Int,Int)

a :: IO ()
a = do
  input <- readInput "input.txt"
  mainA input

b :: IO ()
b = do
  input <- readInput "input.txt"
  mainB input

test1 :: IO ()
test1 = do
  input <- readInput "test1.txt"
  mainA input
test2 :: IO ()
test2 = do
  input <- readInput "test2.txt"
  mainA input

test3 :: IO ()
test3 = do
  input <- readInput "test3.txt"
  mainA input

test4 :: IO ()
test4 = do
  input <- readInput "test2.txt"
  mainA input

testB :: IO ()
testB = do
  input <- readInput "test4.txt"
  mainB input
----------- Code for part b ------------------
mainB :: [Pos] -> IO ()
mainB ls = do
  let mp' = map (addNeighborMap ls) ls
  let mp = foldl' (\(p,a) (p1,b) -> if (Map.size a) - (Map.size b) >= 0 then (p,a) else (p1,b)) ((0,0),Map.empty) mp'
  let toDestroy = sortBy (\(a',ls) (b,ls') -> compare (calculateAngle a') (calculateAngle b)) $ Map.toList $ snd mp
  print $ toDestroy !! 200
  let (_,[(x,y)]) = toDestroy !! 200
  print $ x * 100 + y

calculateAngle :: Pos -> Float
calculateAngle (x,y) = if angle < 0 then ((2* pi) + angle) else angle
  where
    angle = atan2 (fromIntegral y :: Float) (fromIntegral x :: Float) + (pi/2)

addNeighborMap :: [Pos] -> Pos -> (Pos, Map Pos [Pos])
addNeighborMap ls pos = (pos,foldl' (\mp pos2 -> Map.insertWith (++) (vect pos pos2) [pos2] mp) Map.empty ls)

vect :: Pos -> Pos -> Pos
vect (x,y) (x1,y1) = (x' `div` divisor, y' `div` divisor)
  where
    x' = x1-x
    y' = y1-y
    divisor = if gcd x' y' == 0 then 1 else gcd x' y'

----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: [(Int,Int)] -> IO ()
mainA ls = do
  let maps = map (addNeighborMap ls) ls
  let mp = foldl' (\(p,a) (p1,b) -> if (Map.size a) - (Map.size b) >= 0 then (p,a) else (p1,b)) ((0,0),Map.empty) maps
  print $ (((-1) + ) . Map.size) $ snd mp
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO [(Int, Int)]
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> [(Int,Int)]
formatInput input = concat $ zipWith (\x y -> map (\y' -> (y',x)) y) [0..(length input)] (map (elemIndices '#') (lines input))

