import Data.List
import Data.Maybe

modVal = 20201227
cardPub = 8458505
doorPub = 16050997
subject = 7

----------- Code for part two ------------------
two :: IO ()
two = do
  putStrLn "Not yet implemented"
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  let key = foldl' (\val i -> calc cardPub val) 1 [0.. doorLoopSize]
  let key' = foldl' (\val i -> calc doorPub val) 1 [0.. cardLoopSize]
  print key
  print key'

cardLoopSize = fromJust $ elemIndex cardPub ls
doorLoopSize = fromJust $ elemIndex doorPub ls


ls = iterate (calc subject) 1

calc sub val = mod (val * sub) modVal
----------- Code for part one ------------------

