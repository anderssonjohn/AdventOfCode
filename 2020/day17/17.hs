import Data.Set hiding (map, foldl)
import Prelude hiding (minimum, maximum)
import qualified Prelude (minimum, maximum)
----------- Code for part two ------------------
test2 = do
  let input = fromList $ map (\(x,y,z) -> (x,y,z,0)) $ toList test1Input
  putStrLn $ show $ toList input
  putStrLn $ show $ simulate2 input
  let answer = iterate simulate2 input !! 6
  putStrLn $ show $ length $ toList answer
two :: IO ()
two = do
  let input' = fromList $ map (\(x,y,z) -> (x,y,z,0)) $ toList input
  let answer = iterate simulate2 input' !! 6
  putStrLn $ show $ length $ toList answer

type Pos4D = (Int, Int, Int, Int)

simulate2 :: Set Pos4D -> Set Pos4D
simulate2 before = foldl foldFun before positionsToCheck
  where
    ls = toList before
    maxX = 1 +    (maximum $ map (\(x,_,_,_) -> x) ls)
    minX = (-1) + (minimum $ map (\(x,_,_,_) -> x) ls)
    maxY = 1 +    (maximum $ map (\(_,x,_,_) -> x) ls)
    minY = (-1) + (minimum $ map (\(_,x,_,_) -> x) ls)
    maxZ = 1 +    (maximum $ map (\(_,_,x,_) -> x) ls)
    minZ = (-1) + (minimum $ map (\(_,_,x,_) -> x) ls)
    maxW = 1 +    (maximum $ map (\(_,_,_,x) -> x) ls)
    minW = (-1) + (minimum $ map (\(_,_,_,x) -> x) ls)
    positionsToCheck = [(x, y, z, w) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ], w <- [minW..maxW]]
    countActiveNeighbors (x,y,z,w) = length [1 | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1], x /= x' || y /= y' || z /= z' || w /= w', member (x', y', z', w') before]
    foldFun :: Set Pos4D -> Pos4D -> Set Pos4D
    foldFun set pos = let count = countActiveNeighbors pos in
      if member pos before then
        if count == 2 || count == 3 then
          set
        else
          delete pos set
      else
        if count == 3 then
          insert pos set
        else
          set
----------- Code for part two ------------------
----------- Code for part one ------------------
test1 = do
  let input = test1Input
  putStrLn $ show $ toList input
  putStrLn $ show $ simulate input
  let answer = iterate simulate test1Input !! 1
  putStrLn $ show $ length $ toList answer

one :: IO ()
one = do
  let answer = iterate simulate input !! 6
  putStrLn $ show $ length $ toList answer

simulate :: Set Pos -> Set Pos
simulate before = foldl foldFun before positionsToCheck
  where
    ls = toList before
    maxX = 1 + (maximum $ map (\(x,_,_) -> x) ls)
    minX = (-1) + (minimum $ map (\(x,_,_) -> x) ls)
    maxY = 1 + (maximum $ map (\(_,x,_) -> x) ls)
    minY = (-1) + (minimum $ map (\(_,x,_) -> x) ls)
    maxZ = 1 + (maximum $ map (\(_,_,x) -> x) ls)
    minZ = (-1) + (minimum $ map (\(_,_,x) -> x) ls)
    positionsToCheck = [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
    countActiveNeighbors (x,y,z) = length [1 | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], x /= x' || y /= y' || z /= z', member (x', y', z') before]
    foldFun :: Set Pos -> Pos -> Set Pos
    foldFun set pos = let count = countActiveNeighbors pos in
      if member pos before then
        if count == 2 || count == 3 then
          set
        else
          delete pos set
      else
        if count == 3 then
          insert pos set
        else
          set
----------- Code for part one ------------------

minimum :: (Num a, Ord a) => [a] -> a
minimum [] = 0
minimum n = Prelude.minimum n

maximum :: (Num a, Ord a) => [a] -> a
maximum [] = 0
maximum n = Prelude.maximum n

----------- Input ----------------------------
input = fromList [
    (4, 0, 0),

    (1, 1, 0),
    (4, 1, 0),
    (5, 1, 0),
    (6, 1, 0),

    (1, 2, 0),
    (3, 2, 0),
    (5, 2, 0),
    (6, 2, 0),
    (7, 2, 0),

    (1, 3, 0),
    (6, 3, 0),

    (3, 4, 0),
    (5, 4, 0),
    (7, 4, 0),

    (0, 5, 0),

    (0, 6, 0),
    (1, 6, 0),
    (6, 6, 0),

    (1, 7, 0),
    (2, 7, 0),
    (5, 7, 0),
    (7, 7, 0)
  ]

test1Input = fromList [
  (1, 0, 0),
  (2, 1, 0),
  (0, 2, 0),
  (1, 2, 0),
  (2, 2, 0)
  ]

type Pos = Type
type Type = (Int, Int, Int)
type TYPE = [Type]
