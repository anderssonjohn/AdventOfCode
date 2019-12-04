module Four where

printSum :: IO ()
printSum = do
  (min, max) <- readInput
  print $ length $ generateList min max
  return ()

generateList :: Int -> Int -> [Int]
generateList min max = filter passwordCheck [min..max]

passwordCheck :: Int -> Bool
passwordCheck val = increasing digits && containsDouble digits
  where
    digits = digs val

increasing :: [Int] -> Bool
increasing (x:y:xs)
  | x <= y = increasing (y:xs)
  | otherwise = False
increasing (x:xs) = True

containsDouble :: [Int] -> Bool
containsDouble (x:y:xs)
  | x == y = True
  | otherwise = containsDouble (y:xs)
containsDouble (x:xs) = False

readInput :: IO (Int, Int)
readInput = do
  input <- readFile "resources/input4.txt"
  let (a,(_:b)) = splitAt 6 input
  return (read a, read b)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]