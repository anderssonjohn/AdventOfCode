----------- Code for part two ------------------
two :: IO ()
two = do
  putStrLn "Not yet implemented"
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  putStrLn "Not yet implemented"
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = lines input

type Type = Int
type TYPE = [Type]
