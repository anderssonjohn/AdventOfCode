import Data.List.Split
import Data.List
import Data.Char
type Layer = [Row]
type Row = [Int]

a :: IO ()
a = do
  input <- readInput "input.txt"
  mainA input

b :: IO ()
b = do
  input <- readInput "input.txt"
  mainB input

----------- Code for part b ------------------
mainB :: [Layer] -> IO ()
mainB layers = do
  mapM_ print $ foldl compareLayers (head layers) (tail layers)

compareLayers :: Layer -> Layer -> Layer
compareLayers layer1 layer2 = map (\(a,b) -> compareRow a b) $ zip layer1 layer2

compareRow :: Row -> Row -> Row
compareRow row1 row2 = map (\(a,b) -> if a == 2 then b else a) $ zip row1 row2
----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: [Layer] -> IO ()
mainA layers = do
  let ls = map calc0s layers
  let min = minimum ls
  let layer = layers !! (head (elemIndices min ls))
  print $ length (filter (1 == ) (concat layer)) * length (filter (2 == ) (concat layer))

calc0s :: Layer -> Int
calc0s layer = length $ concatMap (filter (0 == )) layer
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO [Layer]
readInput filePath = do
  input <- readFile filePath
  let noNewLine = take (length input -2) input
  return $ formatInput noNewLine

formatInput :: String -> [Layer]
formatInput input = chunksOf 6 $ chunksOf 25 $ map digitToInt input
