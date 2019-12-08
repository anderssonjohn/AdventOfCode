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

testB1 :: IO ()
testB1 = do
  input <- readInput "test4.txt"
  mainB input

testB2 :: IO ()
testB2 = do
  input <- readInput "test5.txt"
  mainB input

----------- Code for part b ------------------
mainB :: [Int] -> IO ()
mainB ls = do
  let perms = permutations [5,6,7,8,9]
  let programs = map (instantiatePrograms ls) perms

  outputs <- mapM (runUntilHalt 0) programs
  print $ maximum $ concat outputs

-- Returns the list of all outputs from the program until it halts
runUntilHalt :: Int -> [(Int,[Int])] -> IO [Int]
runUntilHalt input lss = do
  (output,ls'@((index,_):_)) <- runIteration lss input
  if index == -1 then return [output] -- An index of -1 indicates the program has halted
  else do
    list <- runUntilHalt output ls'
    return $ (output):list

runIteration :: [(Int,[Int])] -> Int -> IO (Int,[(Int,[Int])])
runIteration lss input = foldM (\(input,newPrgs) (index,ls) -> performProgramB ls index input >>= appendOutput newPrgs) (input,[]) lss

appendOutput :: [(Int,[Int])]-> (Int,(Int,[Int])) -> IO (Int,[(Int,[Int])])
appendOutput list (output,prg) = return (output,list ++ [prg])

instantiatePrograms :: [Int] -> [Int] -> [(Int,[Int])]
instantiatePrograms ls settings = map (\ps -> (2,replaceAtIndex (ls !! 1) ps ls)) settings

{-
  When the program outputs something its execution is stopped.
  It then returns the output, the index to continue at, and the updated program.
  This so that when Amp A outputs a value Amp B can start its execution immediately,
  with the input from A, until it outputs a value which is fed to Amp C, and so on.
  The index has to be saved so the program continues from the same state it stopped.
-}
performProgramB :: [Int] -> Int -> Int -> IO (Int, (Int,[Int]))
performProgramB ls index input = do
    let (modes, op) = splitAt 3 $ genNum $ ls !! index
    case op of
      [_,1] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        let newLs = (replaceAtIndex v3 (v1 + v2) ls)
        performProgramB newLs (index + 4) input
      [_,2] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        let newLs = (replaceAtIndex v3 (v1 * v2) ls)
        performProgramB newLs (index + 4) input
      [_,3] -> do
        let newLs = (replaceAtIndex (ls !! (index + 1)) input ls)
        performProgramB newLs (index + 2) input
      [_,4] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        return (v1,(index + 2,ls))
      [_,5] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 /= 0 then do
          performProgramB ls v2 input
        else performProgramB ls (index + 3) input
      [_,6] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 == 0 then do
          performProgramB ls v2 input
        else performProgramB ls (index + 3) input
      [_,7] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 < v2 then performProgramB (replaceAtIndex v3 1 ls) (index + 4) input
        else performProgramB (replaceAtIndex v3 0 ls) (index + 4) input
      [_,8] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 == v2 then performProgramB (replaceAtIndex v3 1 ls) (index + 4) input
        else performProgramB (replaceAtIndex v3 0 ls) (index + 4) input
      [9,9] -> do
        return (input,(-1,ls))
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
        performProgram (replaceAtIndex (ls !! (index + 1)) input ls) (index + 2) input
      [_,4] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        return v1
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

