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
  let hull = Map.insert (0,0) 1 Map.empty
  (a,b,c,d) <- performProgramB ls 0 0 ((0,0),0) hull 1
  print $ map fst $ Map.toList c
  return ()
----------- Code for part b ------------------
----------- Code for part a ------------------

mainA :: IntMap Integer -> IO ()
mainA ls = do
  (a,b,c,d) <- performProgramB ls 0 0 ((0,0),0) Map.empty 1
  print $ Map.size $  c
  return ()

-- Output, rbase, hull of ship, (index, Modified program)
type Program = (Integer, Integer,Hull,(Int, IntMap Integer))
{-
  When the program outputs something its execution is stopped.
  It then returns the output, the index to continue at, and the updated program.
  This so that when Amp A outputs a value Amp B can start its execution immediately,
  with the input from A, until it outputs a value which is fed to Amp C, and so on.
  The index has to be saved so the program continues from the same state it stopped.
-}

(!) :: IntMap Integer -> Int -> Integer
(!) mapp key = IntMap.findWithDefault 0 key mapp

type Hull = Map Position Integer
type Robot = (Position, Direction)
type Position = (Int,Int)
type Direction = Int -- 0 = up, 1 = right, 2 = down, 3 = left
newPosFn (x,y) 0 = (x, y + 1)
newPosFn (x,y) 1 = (x + 1, y)
newPosFn (x,y) 2 = (x, y -  1)
newPosFn (x,y) 3 = (x - 1, y)
performProgramB :: IntMap Integer -> Int -> Integer -> Robot -> Hull -> Int -> IO Program
performProgramB ls index rbase robot@(pos,dir) hull outputOneOrTwo = do
    let (modes, op) = splitAt 3 $ genNum $ ls ! index
    let [v3,v2,v1] = getValues (index + 1) ls modes rbase
    case op of
      [_,1] -> do
        let newLs = insert (frI v3) (v1 + v2) ls
        performProgramB newLs (index + 4) rbase robot hull outputOneOrTwo
      [_,2] -> do
        let newLs = insert (frI v3) (v1 * v2) ls
        performProgramB newLs (index + 4) rbase robot hull outputOneOrTwo
      [_,3] -> do
        let index1 = calculateIndex (index + 1) ls (last modes) rbase
        -- let index1 = case (last modes) of
        --       2 -> rbase + (ls ! (index + 1))
        --       1 -> (toInteger index) + 1
        --       0 -> ls ! (index + 1)

        let val = Map.findWithDefault 0 pos hull
        let newLs = insert (frI index1) val ls
        performProgramB newLs (index + 2) rbase robot hull outputOneOrTwo
      [_,4] -> do
        -- case v1 of
          -- (-7) -> return (0,rbase,hull,(-1,ls))
          case outputOneOrTwo of
                1 -> do
                  let newOutput = 2
                  let hull' = Map.insert pos v1 hull
                  performProgramB ls (index + 2) rbase robot hull' newOutput
                2 -> do
                  let newOutput = 1
                  let newDir = if v1 == 0 then (dir - 1) `mod` 4 else (dir + 1) `mod` 4
                  let newPos = newPosFn pos newDir
                  performProgramB ls (index + 2) rbase (newPos,newDir) hull newOutput

      [_,5] -> do
        if v1 /= 0 then performProgramB ls (frI v2) rbase robot hull outputOneOrTwo
        else performProgramB ls (index + 3) rbase robot hull outputOneOrTwo
      [_,6] -> do
        if v1 == 0 then performProgramB ls (frI v2) rbase robot hull outputOneOrTwo
        else performProgramB ls (index + 3) rbase robot hull outputOneOrTwo
      [_,7] -> do
        if v1 < v2 then performProgramB (insert (frI v3) 1 ls) (index + 4) rbase robot hull outputOneOrTwo
        else performProgramB (insert (frI v3) 0 ls) (index + 4) rbase robot hull outputOneOrTwo
      [_,8] -> do
        if v1 == v2 then performProgramB (insert (frI v3) 1 ls) (index + 4) rbase robot hull outputOneOrTwo
        else performProgramB (insert (frI v3) 0 ls) (index + 4) rbase robot hull outputOneOrTwo
      [0,9] -> do
        performProgramB ls (index + 2) (rbase + v1) robot hull outputOneOrTwo
      [9,9] -> do
        return (0,rbase,hull,(-1,ls))

frI = fromInteger

-- getValues :: Int -> IntMap Integer -> [Integer] -> Integer-> [Integer]
-- getValues index ls [m3,m2,m1] rbase = [index3, ls ! (frI index2), ls ! (frI index1)]
--   where
--     index1 = case m1 of
--       2 -> rbase + (ls ! index)
--       1 -> toInteger index
--       0 -> ls ! index
--     index2 = case m2 of
--       2 -> rbase + (ls ! (index + 1))
--       1 -> toInteger $ index + 1
--       0 -> ls ! (index + 2)
--     index3 = case m3 of
--       2 -> rbase + (ls ! (index + 2))
--       1 -> toInteger $ index + 2
--       0 -> ls ! (index + 2)

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

