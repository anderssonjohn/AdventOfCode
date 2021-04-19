import Data.List
import qualified Data.Map as M
import Data.Maybe (isJust)
import GHC.Exts (sortWith)

type Pos = (Int, Int)
type Board = M.Map Pos Piece
type Row = [Piece]
type Piece = [String]
----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let board' = buildBoard $ map fst input
  let board = sortWith (\((x,y), _) -> (negate y,x)) $ M.assocs $ removeEdges board'

  let chunkSize = 12

  let c = (map . map) snd $ chunksOf chunkSize $ board
  let d = concat $ map (foldl1 fnnn) c

  let permutations =  piecePerms d
  let ls' = map seaMonsters $ map concat permutations

  let seaMonsterCount = maximum ls'

  let totalSq = length $ filter (== '#') $ concat $ head permutations
  let monsterSq = length $ filter (== '#') $ concat seaMonster

  print $ totalSq - monsterSq * seaMonsterCount

fnnn = zipWith (++)

fnn :: ([a] -> b) -> [a] -> [b]
fnn f [] = []
fnn f ls = f ls : fnn f (tail ls)

seaMonster = ["                  # ",
              "#    ##    ##    ###",
              " #  #  #  #  #  #   "]

seaMonsters :: String -> Int
seaMonsters inp = length $ filter (== True) $ fnn (seaMon inds) inp

i = 77
inds = [i, 4, 0, 4, 0, 4, 0, 0, i, 2, 2, 2, 2, 2]

seaMon :: [Int] -> String -> Bool
seaMon [] (a:as) = a == '#'
seaMon _ [] = False
seaMon (i:is) (a:as) = a == '#' && seaMon is (drop i as)

removeEdges :: Board -> Board
removeEdges = M.map rm
  where
    rm = map init . map tail . init . tail

buildBoard :: [Piece] -> Board
buildBoard (a:as) = buildBoard' as $ M.insert (0, 0) a M.empty

buildBoard' :: [Piece] -> Board -> Board
buildBoard' [] a = a
buildBoard' (a:ls) board = returnValue
  where
    boardLs = M.assocs board
    newLs = filter isJust $ concatMap (\(k, val) -> map (\p -> doMatch p k val) perms) boardLs
    perms = piecePerms a
    returnValue = fn newLs
    fn [] = buildBoard' (ls ++ [a]) board
    fn as = let Just (pos, piece) = head as in buildBoard' ls $ M.insert pos piece board


doMatch :: Piece -> Pos -> Piece -> Maybe (Pos, Piece)
doMatch newPiece (x,y) mapPiece = getMatch newPiece
  where
    getMatch :: Piece -> Maybe (Pos, Piece)
    getMatch a
      | up a == down mapPiece = Just ((x, y-1), a)
      | down a == up mapPiece = Just ((x, y+1), a)
      | left a == right mapPiece = Just ((x+1, y), a)
      | right a == left mapPiece = Just ((x-1, y), a)
      | otherwise = Nothing

up, down, left, right :: Piece -> String
up = head
down = last
left = map head
right = map last
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let corner = filter (\a -> length (filter (== True) (map (matches a) (filter (/= a) input))) == 2) input
  print $ product $ map snd corner

rotate :: [[a]] -> [[a]]
rotate ls = map reverse $ transpose ls

matches :: TYPE -> TYPE -> Bool
matches (a,_) (b,_) = or $ map (matches' b) (piecePerms a)

matches' :: Piece -> Piece -> Bool
matches' a b =
  up a == down b ||
  down a == up b ||
  left a == right b ||
  right a == left b

piecePerms :: [[a]] -> [[[a]]]
piecePerms a = (take 4 (iterate rotate a) ++ take 4 (iterate rotate (transpose a)))
----------- Code for part one ------------------
----------- Input ----------------------------
readTest :: IO TYPE'
readTest = do
  input <- readFile "test1.txt"
  return $ formatInput input

readInput :: IO TYPE'
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE'
formatInput input = zip bits ids
  where
    lines' :: [Piece]
    lines' = chunksOf 12 $ lines input
    ids :: [Int]
    ids = map (read . (take 4) . (drop 5) . head) lines'
    bits :: [Piece]
    bits = map ((take 10) . (drop 1)) lines'


type TYPE = (Piece, Int)
type TYPE' = [TYPE]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n a = take n a : chunksOf n (drop n a)
