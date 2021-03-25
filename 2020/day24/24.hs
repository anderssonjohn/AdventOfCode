import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

type Position = (Int, Int)
type Floor = M.Map Position Bool -- True = Black, False = White
data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast
  deriving Show

type Path = [Direction]



----------- Code for part two ------------------
two :: Int -> IO ()
two n = do
  directions <- readInput
  let floor = foldl followPath M.empty directions
  -- let startingBlacks = M.filter (== True) floor
  let endFloor = iterate flipFloor floor !! n
  print $ length $ M.filter (== True) endFloor


flipFloor :: Floor -> Floor
flipFloor floor = newFloor
  where
    appendNeighbors = foldl' (\m pos -> M.insertWith (||) pos False m) floor $ concatMap neighbors $ M.keys $ M.filter (== True) floor
    newFloor = M.mapWithKey mapFn appendNeighbors
    mapFn :: Position -> Bool -> Bool
    mapFn pos True = let bn = blackNeighbors pos in not $ bn == 0 || bn > 2
    mapFn pos False = blackNeighbors pos == 2
    blackNeighbors :: Position -> Int
    blackNeighbors pos = length $ M.filter (== True) $ M.restrictKeys floor $ S.fromList $ neighbors pos -- M.filterWithKey (\k a -> k `elem` neighbors pos && a) floor
    neighbors :: Position -> [Position]
    neighbors (x, y) = (x + 2, y):(x - 2, y):[(x', y') | x' <- [x - 1, x + 1], y' <- [y - 1, y + 1]]
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  directions <- readInput
  let floor = foldl followPath M.empty directions
  let blacks = M.filter (== True) floor
  print $ length blacks

followPath :: Floor -> Path -> Floor
followPath floor path = newFloor
  where
    startingPosition = (0, 0)
    move :: Position -> Direction -> Position
    move (x, y) East = (x + 2, y)
    move (x, y) West = (x - 2, y)
    move (x, y) SouthEast = (x + 1, y - 1)
    move (x, y) SouthWest = (x - 1, y - 1)
    move (x, y) NorthEast = (x + 1, y + 1)
    move (x, y) NorthWest = (x - 1, y + 1)
    finalPosition = foldl move startingPosition path
    newFloor = M.insertWith (/=) finalPosition True floor -- True is always inserted, meaning flip to black. If the tile is already black, flip it to white, else make it black.

----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO [Path]
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> [Path]
formatInput input = map dir $ lines input
  where
    dir :: String -> Path
    dir [] = []
    dir ('e':xs) = East : dir xs
    dir ('w':xs) = West : dir xs
    dir ('s':'e':xs) = SouthEast : dir xs
    dir ('s':'w':xs) = SouthWest : dir xs
    dir ('n':'e':xs) = NorthEast : dir xs
    dir ('n':'w':xs) = NorthWest : dir xs
