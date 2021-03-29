import Data.List

----------- Code for part two ------------------
two :: IO ()
two = do
  putStrLn "Not yet implemented"
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let corner = filter (\a -> length (filter (== True) (map (matches a) (filter (/= a) input))) == 2) input
  print $ product $ map snd corner
----------- Code for part one ------------------

rotate :: Type -> Type
rotate ls = map reverse $ transpose ls

getMatch :: TYPE -> TYPE -> TYPE
getMatch (a,id) (b,_) = (match, id)
  where
    ls = (take 4 (iterate rotate a) ++ take 4 (iterate rotate (transpose a)))
    match = head $ dropWhile (matches' b) ls

matches :: TYPE -> TYPE -> Bool
matches (a,_) (b,_) = or $ map (matches' b) (take 4 (iterate rotate a) ++ take 4 (iterate rotate (transpose a)))

matches' :: Type -> Type -> Bool
matches' a b =
  head a == head b ||
  map head a == map head b ||
  last a == last b ||
  map last a == map last b

----------- Input ----------------------------
readInput :: IO TYPE'
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE'
formatInput input = zip bits ids
  where
    lines' :: [Type]
    lines' = chunksOf 12 $ lines input
    ids :: [Int]
    ids = map (read . (take 4) . (drop 5) . head) lines'
    bits :: [Type]
    bits = map ((take 10) . (drop 1)) lines'


type Type = [String]
type TYPE = (Type, Int)
type TYPE' = [TYPE]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n a = take n a : chunksOf n (drop n a)
