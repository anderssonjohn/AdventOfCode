
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
  mainB input


----------- Code for part b ------------------
mainB :: Input -> IO ()
mainB input = do
  let a = foldl (\curr right -> curr * countTreesB 0 right input) 1 [1, 3, 5, 7]
  let down2 = countTreesB 0 1 $ map fst $ filter (even . snd) (zip input [0..])
  putStrLn $ show $ a * down2

countTreesB :: Int -> Int -> [String] -> Int
countTreesB _ _ [] = 0
countTreesB xPos stepsRight (x:xs) =
  if x !! xPos == '#' then
    1 + countTreesB (xPos + stepsRight) stepsRight xs
  else
    0 + countTreesB (xPos + stepsRight) stepsRight xs

----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: Input -> IO ()
mainA input = do
  putStrLn $ show $ countTrees 0 input

countTrees :: Int -> [String] -> Int
countTrees _ [] = 0
countTrees xIndex (x:xs) =
  if x !! xIndex == '#' then
    1 + countTrees (xIndex + 3) xs
  else
    0 + countTrees (xIndex + 3) xs
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO Input
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> Input
formatInput input = map (concat . repeat) $ lines input

type Type = String
type Input = [Type]
