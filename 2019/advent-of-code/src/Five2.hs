module Five2 where

import Data.List.Split

printSum :: IO ()
printSum = do
  program <- readInputs
  performProgram program 0

readInputs :: IO [Int]
readInputs = do
    input <- readFile "resources/input5.txt"
    return $ map read $ splitOn "," (head $ lines input)

readInput2 :: IO [Int]
readInput2 = do
  input <- readFile "resources/input5.3.txt"
  return $ ((map read) . lines) input

performProgram :: [Int] -> Int -> IO ()
performProgram ls index = do
    let (modes, op) = splitAt 3 $ genNum $ ls !! index
    case op of
      [_,1] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        performProgram (replaceAtIndex v3 (v1 + v2) ls) (index + 4)
      [_,2] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        performProgram (replaceAtIndex v3 (v1 * v2) ls) (index + 4)
      [_,3] -> do
        [val] <- readInput2
        performProgram (replaceAtIndex (ls !! (index + 1)) val ls) (index + 2)
      [_,4] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        print $ v1
        performProgram ls (index + 2)
      [_,5] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 /= 0 then performProgram ls v2
        else performProgram ls (index + 3)
      [_,6] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 == 0 then performProgram ls v2
        else performProgram ls (index + 3)
      [_,7] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 < v2 then performProgram (replaceAtIndex v3 1 ls) (index + 4)
        else performProgram (replaceAtIndex v3 0 ls) (index + 4)
      [_,8] -> do
        let [v3,v2,v1] = getValues (index + 1) ls modes
        if v1 == v2 then performProgram (replaceAtIndex v3 1 ls) (index + 4)
        else performProgram (replaceAtIndex v3 0 ls) (index + 4)
      [9,9] ->
        return ()

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
