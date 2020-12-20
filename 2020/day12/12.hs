----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let (_, (east, south)) = foldl calculateRoute' ((10, -1), (0,0)) input
  putStrLn $ show $ (abs east) + (abs south)

calculateRoute' :: (Pos, Pos) -> Type -> (Pos, Pos)
calculateRoute' (currentDirection@(e, s), pos@(east, south)) (dir, amount) = case dir of
    'F' -> (currentDirection, (east + e * amount, amount * s + south))
    'E' -> ((e + amount, s), pos)
    'S' -> ((e, s + amount), pos)
    'W' -> ((e - amount, s), pos)
    'N' -> ((e, s - amount), pos)
    'L' -> (turnLeft' currentDirection amount, pos)
    'R' -> (turnRight' currentDirection amount, pos)

turnLeft' :: Pos -> Int -> Pos
turnLeft' (e, s) 90 = (s, -e)
turnLeft' (e, s) 180 = (-e, -s)
turnLeft' (e, s) 270 = (-s, e)

turnRight' :: Pos -> Int -> Pos
turnRight' (e, s) 90 = (-s, e)
turnRight' (e, s) 180 = (-e, -s)
turnRight' (e, s) 270 = (s, -e)
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let (_, (east, south)) = foldl calculateRoute ('E', (0,0)) input

  putStrLn $ show $ (abs east) + (abs south)

calculateRoute :: (Direction, Pos) -> Type -> (Direction, Pos)
calculateRoute (currentDirection, pos@(east, south)) (dir, amount) = case dir of
    'F' -> (currentDirection, moveForward (currentDirection, amount) pos)
    'E' -> (currentDirection, (east + amount, south))
    'S' -> (currentDirection, (east, south + amount))
    'W' -> (currentDirection, (east - amount, south))
    'N' -> (currentDirection, (east, south - amount))
    'L' -> (turnLeft currentDirection amount, pos)
    'R' -> (turnRight currentDirection amount, pos)

moveForward :: Type -> Pos -> Pos
moveForward ('E', amount) (east, south) = (east + amount, south)
moveForward ('S', amount) (east, south) = (east, south + amount)
moveForward ('W', amount) (east, south) = (east - amount, south)
moveForward ('N', amount) (east, south) = (east, south - amount)

turnLeft :: Direction -> Int -> Direction
turnLeft 'E' 90 = 'N'
turnLeft 'E' 180 = 'W'
turnLeft 'E' 270 = 'S'
turnLeft 'S' 90 = 'E'
turnLeft 'S' 180 = 'N'
turnLeft 'S' 270 = 'W'
turnLeft 'W' 90 = 'S'
turnLeft 'W' 180 = 'E'
turnLeft 'W' 270 = 'N'
turnLeft 'N' 90 = 'W'
turnLeft 'N' 180 = 'S'
turnLeft 'N' 270 = 'E'

turnRight :: Direction -> Int -> Direction
turnRight 'E' 90 = 'S'
turnRight 'E' 180 = 'W'
turnRight 'E' 270 = 'N'
turnRight 'S' 90 = 'W'
turnRight 'S' 180 = 'N'
turnRight 'S' 270 = 'E'
turnRight 'W' 90 = 'N'
turnRight 'W' 180 = 'E'
turnRight 'W' 270 = 'S'
turnRight 'N' 90 = 'E'
turnRight 'N' 180 = 'S'
turnRight 'N' 270 = 'W'
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = map lineToType $ lines input
  where
    lineToType (x:xs) = (x, read xs)

type Direction = Char
type Pos = (Int, Int)

type Type = (Direction, Int)
type TYPE = [Type]
