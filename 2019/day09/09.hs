import Data.List.Split
import Data.List hiding ((!!), insert)
import Prelude hiding ((!!))
import Data.IntMap (IntMap, insert)
import qualified Data.IntMap as IntMap

main :: IO ()
main = b

a :: IO ()
a = do
  input <- readInput "input.txt"
  mainA input

b :: IO ()
b = do
  input <- readInput "input.txt"
  mainB input

sune :: IO ()
sune = do
  input <- readInput "sune.txt"
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
  (a,b,c) <- performProgramB ls 0 2 0
  return ()
----------- Code for part b ------------------
----------- Code for part a ------------------

mainA :: IntMap Integer -> IO ()
mainA ls = do
  (a,b,c) <- performProgramB ls 0 1 0
  return ()

type Program = (Integer, Integer, (Int, IntMap Integer))
{-
  When the program outputs something its execution is stopped.
  It then returns the output, the index to continue at, and the updated program.
  This so that when Amp A outputs a value Amp B can start its execution immediately,
  with the input from A, until it outputs a value which is fed to Amp C, and so on.
  The index has to be saved so the program continues from the same state it stopped.
-}

(!) :: IntMap Integer -> Int -> Integer
(!) mapp key = IntMap.findWithDefault 0 key mapp

performProgramB :: IntMap Integer -> Int -> Integer -> Integer -> IO Program
performProgramB ls index input rbase = do
    let (modes, op) = splitAt 3 $ genNum $ ls ! index
    let [v3,v2,v1] = getValues (index + 1) ls modes rbase
    case op of
      [_,1] -> do
        let newLs = insert (frI v3) (v1 + v2) ls
        performProgramB newLs (index + 4) input rbase
      [_,2] -> do
        let newLs = (insert (frI v3) (v1 * v2) ls)
        performProgramB newLs (index + 4) input rbase
      [_,3] -> do
        let index1 = calculateIndex (index + 1) ls (last modes) rbase
        let newLs = (insert (frI index1) input ls)
        performProgramB newLs (index + 2) input rbase
      [_,4] -> do
        print v1
        performProgramB ls (index + 2) input rbase
      [_,5] -> do
        if v1 /= 0 then do
          performProgramB ls (frI v2) input rbase
        else performProgramB ls (index + 3) input rbase
      [_,6] -> do
        if v1 == 0 then do
          performProgramB ls (frI v2) input rbase
        else performProgramB ls (index + 3) input rbase
      [_,7] -> do
        if v1 < v2 then performProgramB (insert (frI v3) 1 ls) (index + 4) input rbase
        else performProgramB (insert (frI v3) 0 ls) (index + 4) input rbase
      [_,8] -> do
        if v1 == v2 then performProgramB (insert (frI v3) 1 ls) (index + 4) input rbase
        else performProgramB (insert (frI v3) 0 ls) (index + 4) input rbase
      [0,9] -> do
        performProgramB ls (index + 2) input (rbase + v1)
      [9,9] -> do
        return (input,rbase,(-1,ls))

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
