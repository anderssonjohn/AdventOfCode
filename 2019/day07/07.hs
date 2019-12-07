import Data.List.Split
import Data.List
import Control.Monad

a :: IO ()
a = do
  input <- readInput "input.txt"
  mainA input

b :: IO ()
b = do
  input <- readInput "input.txt"
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

test2 :: IO ()
test2 = do
  input <- readInput "testInput2.txt"
  mainB input


----------- Code for part b ------------------
mainB :: [Int] -> IO ()
mainB input = do
  return ()
----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: [Int] -> IO ()
mainA ls = do
  let perms = permutations [0,1,2,3,4]
  results <- mapM (foldM (\int input -> performFold ls int input) 0) perms
  print $ maximum results

performFold :: [Int] -> Int -> Int -> IO (Int)
performFold ls input phaseSetting = do
  let newLs = (replaceAtIndex (ls !! 1) phaseSetting ls)
  performProgram newLs 2 input

----------- Code for part a ------------------
-- readProgramInput :: IO [Int]
-- readProgramInput = return [5] -- Change to [1] to run part a


performProgram :: [Int] -> Int -> Int -> IO (Int)
performProgram ls index input = do
    let (modes, op) = splitAt 3 $ genNum $ ls !! index
    case op of
      [_,1] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        performProgram (replaceAtIndex v3 (v1 + v2) ls) (index + 4) input
      [_,2] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        performProgram (replaceAtIndex v3 (v1 * v2) ls) (index + 4) input
      [_,3] -> do
        -- [val] <- readProgramInput
        -- performProgram (replaceAtIndex (ls !! (index + 1)) val ls) (index + 2)
        performProgram (replaceAtIndex (ls !! (index + 1)) input ls) (index + 2) input
      [_,4] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        return v1
        -- performProgram ls (index + 2)
      [_,5] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 /= 0 then performProgram ls v2 input
        else performProgram ls (index + 3) input
      [_,6] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 == 0 then performProgram ls v2 input
        else performProgram ls (index + 3) input
      [_,7] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 < v2 then performProgram (replaceAtIndex v3 1 ls) (index + 4) input
        else performProgram (replaceAtIndex v3 0 ls) (index + 4) input
      [_,8] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 == v2 then performProgram (replaceAtIndex v3 1 ls) (index + 4) input
        else performProgram (replaceAtIndex v3 0 ls) (index + 4) input
      [9,9] ->
        return (-1)


getValues :: Int -> [Int] -> [Int] -> [Int]
getValues index ls [m3,m2,m1] = [index3, ls !! index2, ls !! index1]
  where
    index1 = if m1 == 1 then index else ls !! index
    index2 = if m2 == 1 then index + 1 else ls !! (index + 1)
    index3 = if m3 == 1 then index + 2 else ls !! (index + 2)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b)
  where
    (a, (_:b)) = splitAt n ls

genNum :: Int -> [Int]
genNum x = [mod (x `div` 10000) 10,
            mod (x `div` 1000) 10,
            mod (x `div` 100) 10,
            mod (x `div` 10) 10,
            mod x 10]

----------- Input ----------------------------
readInput :: String -> IO [Int]
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> [Int]
formatInput input = map read $ splitOn "," (head (lines input))

