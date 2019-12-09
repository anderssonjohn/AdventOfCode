import Data.List.Split
import Data.List hiding ((!!))
import Prelude hiding ((!!))

-- TODO: use IntMap

main :: IO ()
main = a

a :: IO ()
a = do
  input <- readInput "input.txt"
  mainA input

b :: IO ()
b = do
  input <- readInput "sune.txt"
  mainB input

testA1 :: IO ()
testA1 = do
  input <- readInput "test1.txt"
  mainA input
testA2 :: IO ()
testA2 = do
  input <- readInput "test2.txt"
  mainA input
testA3 :: IO ()
testA3 = do
  input <- readInput "test3.txt"
  mainA input



----------- Code for part b ------------------
mainB :: [Integer] -> IO ()
mainB ls' = do
  let ls = ls' ++ (replicate 10000 0)
  (a,b,c) <- performProgramB ls 0 2 0
  return ()
----------- Code for part b ------------------
----------- Code for part a ------------------

mainA :: [Integer] -> IO ()
mainA ls' = do
  let ls = ls' ++ (replicate 10000 0)
  (a,b,c) <- performProgramB ls 0 1 0
  return ()

type Program = (Integer, Integer, (Integer, [Integer]))
{-
  When the program outputs something its execution is stopped.
  It then returns the output, the index to continue at, and the updated program.
  This so that when Amp A outputs a value Amp B can start its execution immediately,
  with the input from A, until it outputs a value which is fed to Amp C, and so on.
  The index has to be saved so the program continues from the same state it stopped.
-}

(!!) = (genericIndex)
performProgramB :: [Integer] -> Integer -> Integer -> Integer -> IO Program
performProgramB ls index input rbase = do
    let (modes, op) = splitAt 3 $ genNum $ ls !! index
    let [v3,v2,v1] = getValues (index + 1) ls modes rbase
    case op of
      [_,1] -> do
        let newLs = (replaceAtIndex v3 (v1 + v2) ls)
        performProgramB newLs (index + 4) input rbase
      [_,2] -> do
        let newLs = (replaceAtIndex v3 (v1 * v2) ls)
        performProgramB newLs (index + 4) input rbase
      [_,3] -> do
        let index1 = case (last modes) of
                      2 -> rbase + (ls `genericIndex` (index + 1))
                      1 -> index + 1
                      0 -> ls `genericIndex` (index + 1)
        let newLs = (replaceAtIndex index1 input ls)
        performProgramB newLs (index + 2) input rbase
      [_,4] -> do
        print v1
        performProgramB ls (index + 2) input rbase
      [_,5] -> do
        if v1 /= 0 then do
          performProgramB ls v2 input rbase
        else performProgramB ls (index + 3) input rbase
      [_,6] -> do
        if v1 == 0 then do
          performProgramB ls v2 input rbase
        else performProgramB ls (index + 3) input rbase
      [_,7] -> do
        if v1 < v2 then performProgramB (replaceAtIndex v3 1 ls) (index + 4) input rbase
        else performProgramB (replaceAtIndex v3 0 ls) (index + 4) input rbase
      [_,8] -> do
        if v1 == v2 then performProgramB (replaceAtIndex v3 1 ls) (index + 4) input rbase
        else performProgramB (replaceAtIndex v3 0 ls) (index + 4) input rbase
      [0,9] -> do
        performProgramB ls (index + 2) input (rbase + v1)
      [9,9] -> do
        return (input,rbase,(-1,ls))

getValues :: Integer -> [Integer] -> [Integer] -> Integer-> [Integer]
getValues index ls [m3,m2,m1] rbase = [index3, ls !! index2, ls !! index1]
  where
    index1 = case m1 of
      2 -> rbase + (ls !! index)
      1 -> index
      0 -> ls !! index
    index2 = case m2 of
      2 -> rbase + (ls !! (index + 1))
      1 -> index + 1
      0 -> ls !! (index + 2)
    index3 = case m3 of
      2 -> rbase + (ls !! (index + 2))
      1 -> index + 2
      0 -> ls !! (index + 2)

replaceAtIndex :: Integer -> a -> [a] -> [a]
replaceAtIndex n' item ls = a ++ (item:b)
  where
    n = fromIntegral n'
    (a, (_:b)) = splitAt n ls

genNum :: Integer -> [Integer]
genNum x = [mod (x `div` 10000) 10,
            mod (x `div` 1000) 10,
            mod (x `div` 100) 10,
            mod (x `div` 10) 10,
            mod x 10]
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO [Integer]
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> [Integer]
formatInput input = map read $ splitOn "," (head (lines input))
