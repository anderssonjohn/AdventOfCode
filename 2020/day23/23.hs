import Data.List
----------- Code for part two ------------------
main = two
two :: IO ()
two = do
  let newInput = input ++ [10..1000000]
  -- let finalState = funRepeat 100 moveCups2 newInput
  -- let finalState = foldl' (\inp _ -> foldl' (\input _ -> moveCups2 input) inp [1..100]) newInput [1..10000]
  let finalState = foldl' (\inp _ -> moveCups2 inp) newInput [1..10000000]

  let (Just index1) = elemIndex 1 finalState
  putStrLn $ show $ (finalState !! (index1 + 1)) * (finalState !! (index1 + 2))

moveCups2 :: [Int] -> [Int]
moveCups2 (c:m1:m2:m3:ls) = afterMove
  where
    afterMove | null newEnd = (d:move) ++ newStart ++ [c]
              | otherwise = newEnd ++ (d:move) ++ newStart ++ [c]
    (newEnd, (d:newStart)) = break (== destination) ls
    move = [m1, m2, m3]
    destination = dest' (c - 1)
    dest' n | n == 0 = dest' 999999
            | n `elem` move = dest' (n - 1)
            | n == c = dest' (n - 1)
            | otherwise = n
----------- Code for part two ------------------
----------- Code for part one ------------------
test1 = putStrLn $ concatMap show $ tail $ funRepeat 100 moveCups testInput1
one :: IO ()
one = do
  putStrLn $ concatMap show $ tail $ funRepeat 100 moveCups input

funRepeat n f = foldr (.) id $ replicate n f

moveCups :: [Int] -> [Int]
moveCups (c:m1:m2:m3:ls) = afterMove
  where
    afterMove | null newEnd = (d:move) ++ newStart ++ [c]
              | otherwise = newEnd ++ (d:move) ++ newStart ++ [c]
    (newEnd, (d:newStart)) = break (== destination) ls
    move = [m1, m2, m3]
    destination = dest' (c - 1)
    dest' n | n == 0 = dest' 9
            | n `elem` move = dest' (n - 1)
            | n == c = dest' (n - 1)
            | otherwise = n

----------- Code for part one ------------------


----------- Input ----------------------------
input :: [Int]
input = [4, 7, 6, 1, 3, 8, 2, 5, 9]
testInput1 :: [Int]
testInput1 = [3, 8, 9, 1, 2, 5, 4, 6, 7]
