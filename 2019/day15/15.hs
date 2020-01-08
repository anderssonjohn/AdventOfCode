
import Control.Monad
import Data.List.Split
import Data.List hiding ((!!), insert)
import Prelude hiding ((!!))
import Data.IntMap (IntMap, insert)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map


import Control.Monad.State

main :: IO ()
main = a

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
  input <- readInput "test1.txt"
  mainA input

----------- Code for part b ------------------
mainB :: IntMap Integer -> IO ()
mainB ls = do
  putStrLn "Not yet implemented"
----------- Code for part b ------------------
----------- Code for part a ------------------

mainA :: IntMap Integer -> IO ()
mainA ls = do
  let env = emptyEnv ls
  let state = execState performProgram env
  print $ output state
  return ()

(!) :: IntMap Integer -> Int -> Integer
(!) mapp key = IntMap.findWithDefault 0 key mapp

type World = Map Position Integer
type Position = (Int,Int)
type Direction = Int

emptyEnv :: IntMap Integer -> Env
emptyEnv prg = Env {
  rbase = 0,
  index = 0,
  output = [],
  program = prg,
  world = Map.empty,
  currPos = (0,0),
  newPos = (0,0)
}

data Env = Env {
  rbase :: Integer,
  index :: Int,
  output :: [Integer],
  program :: IntMap Integer,
  world :: World,
  currPos :: Position,
  newPos :: Position
}

addPos :: Position -> Position -> Position
addPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

getDirection :: Direction -> Position
getDirection dir
  | dir == north = (0,1)
  | dir == south = (0,-1)
  | dir == west = (-1, 0)
  | dir == east = (1,0)

setIndex :: Integer -> State Env ()
setIndex i = modify $ \s -> s{index = (frI i)}

setNewPos :: Direction -> State Env ()
setNewPos dir = do
  let dirPos = getDirection dir
  oldPos <- gets newPos
  modify $ \s -> s{newPos = addPos oldPos dirPos}

setCurrPos :: Direction -> State Env ()
setCurrPos dir = do
  let dirPos = getDirection dir
  oldPos <- gets currPos
  modify $ \s -> s{newPos = addPos oldPos dirPos}

updateRbase :: Integer -> State Env ()
updateRbase i = do
  r <- gets rbase
  modify $ \s -> s{rbase = r + i}

incrementIndex :: Int -> State Env ()
incrementIndex i = do
  index' <- gets index
  modify $ \s -> s{index = index' + i}

updatePrg :: Int -> Integer -> State Env ()
updatePrg ind val = do
  prg <- gets program
  modify $ \s -> s{program = IntMap.insert ind val prg}

saveOutput :: Integer -> State Env ()
saveOutput int = do
  outpt <- gets output
  modify $ \s -> s{output = int:outpt}

input :: State Env Integer
input = do
  mapp <- gets world
  return 2

findUnexplored :: World -> Position -> Position
findUnexplored world pos
  | Map.notMember (addPos (getDirection north) pos) world = (addPos (getDirection north) pos)
  | Map.notMember (addPos (getDirection south) pos) world   = (addPos (getDirection south) pos)
  | Map.notMember (addPos (getDirection west) pos) world   = (addPos (getDirection west) pos)
  | Map.notMember (addPos (getDirection east) pos) world   = (addPos (getDirection east) pos)

north = 1
south = 2
west = 3
east = 4

performProgram :: State Env ()
performProgram = do
    ind <- gets index
    prg <- gets program
    rbas <- gets rbase
    let (modes, op) = splitAt 3 $ genNum $ prg ! ind
    let [v3,v2,v1] = getValues (ind + 1) prg modes rbas
    case op of
      [_,1] -> do
        updatePrg (frI v3) (v1 + v2)
        incrementIndex 4
        performProgram
      [_,2] -> do
        updatePrg (frI v3) (v1 * v2)
        incrementIndex 4
        performProgram
      [_,3] -> do
        let index1 = calculateIndex (ind + 1) prg (last modes) rbas
        inpt <- input
        updatePrg (frI index1) inpt
        incrementIndex 2
        performProgram
      [_,4] -> do
        saveOutput v1
        incrementIndex 2
        performProgram
      [_,5] -> do
        if v1 /= 0 then setIndex v2
        else incrementIndex 3
        performProgram
      [_,6] -> do
        if v1 == 0 then setIndex v2
        else incrementIndex 3
        performProgram
      [_,7] -> do
        if v1 < v2 then updatePrg (frI v3) 1
        else updatePrg (frI v3) 0
        incrementIndex 4
        performProgram
      [_,8] -> do
        if v1 == v2 then updatePrg (frI v3) 1
        else updatePrg (frI v3) 0
        incrementIndex 4
        performProgram
      [0,9] -> do
        updateRbase v1
        incrementIndex 2
        performProgram
      [9,9] -> do
        return ()

frI = fromInteger

getValues :: Int -> IntMap Integer -> [Integer] -> Integer-> [Integer]
getValues index ls [m3,m2,m1] rbase = [index3, ls ! (frI index2), ls ! (frI index1)]
  where
    index1 = calculateIndex index ls m1 rbase
    index2 = calculateIndex (index + 1) ls m2 rbase
    index3 = calculateIndex (index + 2) ls m3 rbase

calculateIndex :: Int -> IntMap Integer -> Integer -> Integer -> Integer
calculateIndex index ls mode rbase = case mode of
  2 -> rbase + (ls ! index)
  1 -> toInteger index
  0 -> ls ! index

genNum :: Integer -> [Integer]
genNum x = [mod (x `div` 10000) 10,
            mod (x `div` 1000) 10,
            mod (x `div` 100) 10,
            mod (x `div` 10) 10,
            mod x 10]
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO (IntMap Integer)
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> IntMap Integer
formatInput input = intMap
  where
    intMap = IntMap.fromList ls'
    ls = map read $ splitOn "," (head (lines input))
    ls' = zip [0..] ls
