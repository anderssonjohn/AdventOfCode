
a :: IO ()
a = do
  input <- readInput "input.txt"
  mainA input

b :: IO ()
b = do
  input <- readInput "input.txt"
  mainB input


----------- Code for part b ------------------
mainB :: [Integer] -> IO ()
mainB input = do
  let answer = findProductB input
  putStrLn $ show answer

findProductB :: [Integer] -> Integer
findProductB (x:xs) = if null product then findProductB xs else head product
  where
    product = [x * y * z | y <- xs, z <- xs, (x + y + z) == 2020]

----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: [Integer] -> IO ()
mainA input = do
  putStrLn $ show $ findProduct input

findProduct :: [Integer] -> Integer
findProduct (x:xs) = if null product then findProduct xs else head product
  where
    product = [x * y | y <- xs, x + y == 2020]
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO [Integer]
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> [Integer]
formatInput input = map read (lines input)
