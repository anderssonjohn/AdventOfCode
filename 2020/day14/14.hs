import Data.List
import Data.List.Split
import Data.Char

import qualified Data.IntMap as IntMap

test1 = do
  input' <- readFile "test1.txt"
  let input = formatInput input'
  let answer = foldl fun IntMap.empty input
  putStrLn $ show $ foldl (\curr (k,v) -> curr + (toInteger v)) 0 $ IntMap.toList answer

test2 = do
  input' <- readFile "test2.txt"
  let input = formatInput input'
  let answer = foldl fun' IntMap.empty input
  putStrLn $ show $ foldl (\curr (k,v) -> curr + (toInteger v)) 0 $ IntMap.toList answer

----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let answer = foldl fun' IntMap.empty input
  putStrLn $ show $ foldl (\curr (k,v) -> curr + (toInteger v)) 0 $ IntMap.toList answer

fun' :: IntMap.IntMap Int -> Type -> IntMap.IntMap Int
fun' currentMemory (mask, mems) = foldl calculate currentMemory mems
  where
    calculate map (memPosition, value) = foldl (\cm pos -> IntMap.insert pos (fromBinary value) cm) map (adresses mask memPosition)

adresses :: String -> Int -> [Int]
adresses a b = map fromBinary $ adresses' a (toBinary b)

adresses' :: String -> [Int] -> [[Int]]
adresses' ('0':xs) (y:ys) = map (y :) (adresses' xs ys)
adresses' ('1':xs) (y:ys) = map (1 :)(adresses' xs ys)
adresses' ('X':xs) (y:ys) = (map (0 :) (adresses' xs ys)) ++ (map (1 :) (adresses' xs ys))
adresses' _ a = [a]
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let answer = foldl fun IntMap.empty input
  putStrLn $ show $ foldl (\curr (k,v) -> curr + (toInteger v)) 0 $ IntMap.toList answer

fun :: IntMap.IntMap Int -> Type -> IntMap.IntMap Int
fun currentMemory (mask, mems) = foldl calculate currentMemory mems
  where
    calculate map (memPosition, value) = IntMap.insert memPosition (fromBinary $ binaryAdd mask value) map


fromBinary :: [Int] -> Int
fromBinary ls = fun 0 $ reverse ls
  where
    fun n (0:xs) = fun (n + 1) xs
    fun n (1:xs) = 2 ^ n + fun (n + 1) xs
    fun _ [] = 0

toBinary :: Int -> [Int]
toBinary = go 36 [] where
    go 0 acc _ = acc
    go n acc x = go (n-1) (bit:acc) x' where
        (x', bit) = x `divMod` 2

stringToBinary :: String -> [Int]
stringToBinary = toBinary . read

binaryAdd :: String -> [Int] -> [Int]
binaryAdd ('0':xs) (y:ys) = 0: (binaryAdd xs ys)
binaryAdd ('1':xs) (y:ys) = 1: (binaryAdd xs ys)
binaryAdd ('X':xs) (y:ys) = y:(binaryAdd xs ys)
binaryAdd [] [] = []

----------- Code for part one ------------------
----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = map fun $ drop 1 $ map lines $ splitOn "mask = " input
  where
    fun (x:xs) = (x, map fun' xs)
    fun' str = let Just (a, [b]) = uncons $ map (filter isDigit) $ splitOn "=" str in (read a, stringToBinary b)

type Type = (String, [(Int, [Int])])
type TYPE = [Type]
