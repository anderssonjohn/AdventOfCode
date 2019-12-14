import Debug.Trace
import Control.Monad
import Data.List.Split
import Data.List hiding ((!!), insert)
import Prelude hiding ((!!))
import Data.IntMap (IntMap, insert)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = a

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



----------- Code for part b ------------------
mainB :: IntMap Integer -> IO ()
mainB ls = do
  let ls' = IntMap.insert 0 2 ls
  (a,b,c,d,e) <- performProgramB ls' 0 0 Map.empty 1 (0,0) 0
  mapM_ putStrLn $ formatOutput c
  print d
  return ()

formatOutput :: GameBoard -> [String]
formatOutput mapp = [ [ formatChar (Map.findWithDefault 0 (x,y) mapp) | x <- [(0 ::Int)..37]  ] | y <- [(0 :: Int)..26] ]

determineDirection :: GameBoard -> Integer
determineDirection board = direction
  where
    ball = fst $ fst $ head $ filter ((4 == ) . snd) $ Map.toList board
    player = fst $ fst $ head $ filter ((3 == ) . snd) $ Map.toList board
    direction = if ball == player then 0 else (if ball < player then (-1) else 1)

formatChar :: Integer -> Char
formatChar int = case int of
  0 -> ' '
  1 -> '|'
  2 -> '-'
  3 -> '='
  4 -> 'O'
----------- Code for part b ------------------
----------- Code for part a ------------------

mainA :: IntMap Integer -> IO ()
mainA ls = do
  (a,b,c,d,e) <- performProgramB ls 0 0 Map.empty 1 (0,0) 0
  print $ length $ filter (2 ==) $ map snd $ Map.toList c
  print $ formatOutput c
  return ()

-- Output, rbase, gameBoard of ship, score from playing, (index, Modified program)
type Program = (Integer, Integer,GameBoard,Int,(Int, IntMap Integer))
{-
  When the program outputs something its execution is stopped.
  It then returns the output, the index to continue at, and the updated program.
  This so that when Amp A outputs a value Amp B can start its execution immediately,
  with the input from A, until it outputs a value which is fed to Amp C, and so on.
  The index has to be saved so the program continues from the same state it stopped.
-}

(!) :: IntMap Integer -> Int -> Integer
(!) mapp key = IntMap.findWithDefault 0 key mapp

type GameBoard = Map Position Integer
type Position = (Int,Int)

performProgramB :: IntMap Integer -> Int -> Integer -> GameBoard -> Int -> Position -> Int -> IO Program
performProgramB ls index rbase gameBoard outputIndex outputPosition@(x,y) currentScore = do
    let (modes, op) = splitAt 3 $ genNum $ ls ! index
    let [v3,v2,v1] = getValues (index + 1) ls modes rbase
    case op of
      [_,1] -> do
        let newLs = insert (frI v3) (v1 + v2) ls
        performProgramB newLs (index + 4) rbase gameBoard outputIndex outputPosition currentScore
      [_,2] -> do
        let newLs = insert (frI v3) (v1 * v2) ls
        performProgramB newLs (index + 4) rbase gameBoard outputIndex outputPosition currentScore
      [_,3] -> do
        let direction = determineDirection gameBoard
        let index1 = calculateIndex (index + 1) ls (last modes) rbase
        let newLs = insert (frI index1) direction ls
        performProgramB newLs (index + 2) rbase gameBoard outputIndex outputPosition currentScore
      [_,4] -> do
        case outputIndex of
          1 -> do
            performProgramB ls (index + 2) rbase gameBoard 2 ((frI v1),y) currentScore
          2 -> do
            performProgramB ls (index + 2) rbase gameBoard 3 (x,(frI v1)) currentScore
          3 -> do
            if x == (-1) && y == 0 then performProgramB ls (index + 2) rbase gameBoard 1 (0,0) (frI v1)
              else let newBoard = Map.insert outputPosition v1 gameBoard in performProgramB ls (index + 2) rbase newBoard 1 outputPosition currentScore
      [_,5] -> do
        if v1 /= 0 then performProgramB ls (frI v2) rbase gameBoard outputIndex outputPosition currentScore
        else performProgramB ls (index + 3) rbase gameBoard outputIndex outputPosition currentScore
      [_,6] -> do
        if v1 == 0 then performProgramB ls (frI v2) rbase gameBoard outputIndex outputPosition currentScore
        else performProgramB ls (index + 3) rbase gameBoard outputIndex outputPosition currentScore
      [_,7] -> do
        if v1 < v2 then performProgramB (insert (frI v3) 1 ls) (index + 4) rbase gameBoard outputIndex outputPosition currentScore
        else performProgramB (insert (frI v3) 0 ls) (index + 4) rbase gameBoard outputIndex outputPosition currentScore
      [_,8] -> do
        if v1 == v2 then performProgramB (insert (frI v3) 1 ls) (index + 4) rbase gameBoard outputIndex outputPosition currentScore
        else performProgramB (insert (frI v3) 0 ls) (index + 4) rbase gameBoard outputIndex outputPosition currentScore
      [0,9] -> do
        performProgramB ls (index + 2) (rbase + v1) gameBoard outputIndex outputPosition currentScore
      [9,9] -> do
        return (0,rbase,gameBoard,currentScore,(-1,ls))

frI = fromInteger

getValues :: Int -> IntMap Integer -> [Integer] -> Integer-> [Integer]
getValues index ls [m3,m2,m1] rbase = [index3, ls ! (frI index2), ls ! (frI index1)]
  where
    index1 = calculateIndex index ls m1 rbase
    index2 = calculateIndex (index + 1) ls m2 rbase
    index3 = calculateIndex (index + 2) ls m3 rbase

calculateIndex :: Int -> IntMap Integer -> Integer -> Integer -> Integer
calculateIndex index ls mode rbase = case mode of
  2 -> rbase + (ls ! index)
  1 -> toInteger index
  0 -> ls ! index

genNum :: Integer -> [Integer]
genNum x = [mod (x `div` 10000) 10,
            mod (x `div` 1000) 10,
            mod (x `div` 100) 10,
            mod (x `div` 10) 10,
            mod x 10]
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO (IntMap Integer)
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> IntMap Integer
formatInput input = intMap
  where
    intMap = IntMap.fromList ls'
    ls = map read $ splitOn "," (head (lines input))
    ls' = zip [0..] ls
