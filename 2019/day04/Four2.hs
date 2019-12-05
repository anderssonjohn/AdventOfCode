module Four2 where

printSum :: IO ()
printSum = do
  (min, max) <- readInput
  print $ length $ generateList min max
  return ()

generateList :: Int -> Int -> [Int]
generateList min max = filter (passwordCheck . digs) [min..max]

passwordCheck :: [Int] -> Bool
passwordCheck digits = increasing digits && containsExactDouble digits

increasing :: [Int] -> Bool
increasing (x:y:xs)
  | x <= y = increasing (y:xs)
  | otherwise = False
increasing (x:xs) = True

containsExactDouble :: [Int] -> Bool
containsExactDouble (a:b:c:d:e:f:_) = (ab && not bc) || (not ab && bc && not cd) || (not bc && cd && not de) || (not cd && de && not ef) || (not de && ef)
  where
    ab = a == b
    bc = b == c
    cd = c == d
    de = d == e
    ef = e == f

readInput :: IO (Int, Int)
readInput = do
  input <- readFile "resources/input4.txt"
  let (a,(_:b)) = splitAt 6 input
  return (read a, read b)

digs :: Integral x => x -> [x]
digs x = [mod (x `div` 100000) 10,
          mod (x `div` 10000) 10,
          mod (x `div` 1000) 10,
          mod (x `div` 100) 10,
          mod (x `div` 10) 10,
          mod x 10]