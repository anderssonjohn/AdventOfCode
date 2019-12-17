import Control.Monad
import Data.List
main = test1B

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
test1B :: IO ()
test1B = do
  input <- readInput "test1.txt"
  mainB input

test2B :: IO ()
test2B = do
  input <- readInput "test2.txt"
  mainB input

test3B :: IO ()
test3B = do
  input <- readInput "test3.txt"
  mainB input

test4 :: IO ()
test4 = do
  let input = [1,2,3,4,5,6,7,8]
  mainA input

----------- Code for part b ------------------
mainB :: [Int] -> IO ()
mainB input' = do
  let input = concat $ replicate 10000 input'
  let b = (genPattern (length input))
  print $ length b
  print $ length input
  let a = foldl' (\input' i -> runPhase input' b) input [1..100]
  print $ take 8 a
----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: [Int] -> IO ()
mainA input = do
  let b = (genPattern (length input))
  let a = foldl (\input' i -> runPhase input' b) input [1..100]
  print $ take 8 a

genPattern :: Int -> [[Int]]
genPattern length = [genPattern' x | x <- [1..(length)]]

genPattern' :: Int -> [Int]
genPattern' i = drop 1 (cycle (concat [(replicate i 0), (replicate i 1), (replicate i 0), (replicate i (-1))]))

runPhase :: [Int] -> [[Int]] -> [Int]
runPhase input pattern' = map (zipper input) pattern'

zipper :: [Int] -> [Int] -> Int
zipper input pattern' = (\num -> mod num 10) $ abs $ sum $ zipWith (*) input pattern'
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO [Int]
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> [Int]
formatInput input = digs $ read input

digs :: Integer -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [fromInteger (x `mod` 10)]
