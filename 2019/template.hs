
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
  input <- readInput "testInput1.txt"
  mainA input

test2 :: IO ()
test2 = do
  input <- readInput "testInput2.txt"
  mainB input


----------- Code for part b ------------------
mainB :: TYPE -> IO ()
mainB input = do
  things
----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: TYPE -> IO ()
mainA input = do
  things
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO TYPE
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: [String] -> TYPE
formatInput input = lines input
