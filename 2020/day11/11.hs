import Data.Map hiding (foldl, map, filter)
import qualified Data.Map (map)
import Data.List hiding (insert)
----------- Code for part two ------------------
main = two
two :: IO ()
two = do
  input <- readInput
  let finalState = runIterations' input

  putStrLn $ show $ countOccupied finalState

runIterations' :: TYPE -> TYPE
runIterations' start = if (performIteration' start) === start
  then start
  else runIterations' (performIteration' start)



performIteration' :: TYPE -> TYPE
performIteration' mappp = foldl foldFun mappp indices
  where
    indices :: [Pos]
    indices = [(x,y) | x <- [0..97], y <- [0..92]]
    foldFun :: TYPE -> Pos -> TYPE
    foldFun mapp pos = case (mappp ! pos) of
      Floor -> mapp
      Empty -> if (length (filter (True ==) (map occupiedSeats (seen pos)))) == 0 then insert pos Occupied mapp else mapp
      Occupied -> if (length (filter (True ==) (map occupiedSeats (seen pos)))) >= 5 then insert pos Empty mapp else mapp
    occupiedSeats :: [Pos] -> Bool
    occupiedSeats (pos:xs) = case (mappp ! pos) of
      Floor -> occupiedSeats xs
      Empty -> False
      Occupied -> True
    occupiedSeats [] = False
    seen :: Pos -> [[Pos]]
    seen (x,y) = (map (filter l)) [[addPos (mult * x', mult * y') (x, y) | mult <- [1..100]] | x' <- [(-1)..1], y' <- [(-1)..1], x' /= 0 || y' /= 0]
    l (x, y) = x >= 0 && y >= 0 && x <= 97 && y <= 92

----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let finalState = runIterations input

  putStrLn $ show $ countOccupied finalState


showMap :: TYPE -> String
showMap mapp = unlines $ map lineToString lines'
  where
    lineToString :: [Seat] -> String
    lineToString line = map seatToChar line
    seatToChar Floor = '.'
    seatToChar Empty = 'L'
    seatToChar Occupied = '#'
    lines' :: [[Seat]]
    lines' = (map . map) snd $ groupBy (\((x,_), _) ((x',_), _) -> x == x') (toList mapp)

countOccupied :: TYPE -> Int
countOccupied mapp = length $ filter isOccupied $ map snd $ toList mapp
  where
    isOccupied Occupied = True
    isOccupied _ = False

runIterations :: TYPE -> TYPE
runIterations start = if (performIteration start) === start
  then start
  else runIterations (performIteration start)

(===) :: TYPE -> TYPE -> Bool
map1 === map2 = (toList map1) == (toList map2)

performIteration :: TYPE -> TYPE
performIteration mappp = foldl foldFun mappp indices
  where
    indices = [(x,y) | x <- [0..97], y <- [0..92]]
    foldFun mapp pos = case (mappp ! pos) of
      Floor -> mapp
      Empty -> if (length (filter (True ==) (map occupiedSeats (adjacents pos)))) == 0 then insert pos Occupied mapp else mapp
      Occupied -> if (length (filter (True ==) (map occupiedSeats (adjacents pos)))) >= 4 then insert pos Empty mapp else mapp
    occupiedSeats pos = occupied (mappp ! pos)
    occupied Occupied = True
    occupied _ = False
    adjacents (x,y) = filter l [addPos (x', y') (x, y) | x' <- [(-1)..1], y' <- [(-1)..1], x' /= 0 || y' /= 0]
    l (x, y) = x >= 0 && y >= 0 && x <= 97 && y <= 92

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = foldl fun empty (zip [0..] (map (zip [0..]) (lines input)))
  where
    fun :: Map Pos Seat -> (Int, [(Int, Char)]) -> Map Pos Seat
    fun map (i, line) = foldl (fun' i) map line
    fun' :: Int -> Map Pos Seat -> (Int, Char) -> Map Pos Seat
    fun' i map (j, char) = insert (i, j) (charToSeat char) map

charToSeat :: Char -> Seat
charToSeat '.' = Floor
charToSeat 'L' = Empty
charToSeat '#' = Occupied

data Seat = Floor | Empty | Occupied
  deriving (Eq, Show)
type Pos = (Int, Int)

type Type = Int
type TYPE = Map Pos Seat
