
import Control.Monad
import Data.List.Split
import Data.List hiding ((!!), insert)
import Prelude hiding ((!!))
import Data.IntMap (IntMap, insert)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

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
  -- let state = execState performProgram env
  let world = evalState performProgram env
  print world
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
  -- world = Map.empty,
  world = Map.insert (0, 0) 1 Map.empty,
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

instance Show Env where
  show Env{rbase=rbase, index=index, world=world, currPos=currPos,newPos=newPos}
    = "rbase: " ++ show rbase ++
      "\nindex: " ++ show index ++
      "\nworld: " ++ show world ++
      "\ncurrPos: " ++ show currPos ++
      "\nnewPos: " ++ show newPos ++ "\n\n"


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

setCurrPos :: Position -> State Env ()
setCurrPos pos = modify $ \s -> s{currPos = pos}

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

-- saveOutput :: Integer -> State Env ()
-- saveOutput int = do
--   oldOutput <- gets output
--   modify $ \s -> s{output = int:oldOutput}

-- getInput :: State Env Integer
-- getInput = do
--   mapp <- gets world -- TODO: this is where movement commands to the robot are issued
--   return 2

-- findUnexplored :: World -> Position -> Position
-- findUnexplored world pos
--   | Map.notMember (addPos (getDirection north) pos) world = (addPos (getDirection north) pos)
--   | Map.notMember (addPos (getDirection south) pos) world   = (addPos (getDirection south) pos)
--   | Map.notMember (addPos (getDirection west) pos) world   = (addPos (getDirection west) pos)
--   | Map.notMember (addPos (getDirection east) pos) world   = (addPos (getDirection east) pos)

north, south, west, east :: Direction
north = 1
south = 2
west = 3
east = 4

allDirs :: [Int]
allDirs = [north, south, west, east]

-- Calculates which position to move in order to get from the current position to the new position
getDir :: Position -> Position -> Direction
getDir (x1, y1) (x2, y2)
  | x1 < x2 = north
  | x1 > x2 = south
  | y1 > y2 = west
  | y1 < y2 = east

-- When an input command is wanted, map over all directions which are unexplored and do performProgram on them, then merge the resulting maps.

mapelido :: Integer -> State Env World
mapelido index1 = do
  -- curPos <- trace ("enter mapelido") $ gets currPos
  -- wrld <- trace ("get world, curPos: " ++ show curPos) gets world
  -- let newPos' = trace ("mapping dirs") map (\dir -> addPos curPos (getDirection dir)) allDirs
  -- let newPoss = trace ("filtering") filter (flip Map.notMember wrld) newPos'
  -- startState <- trace ("get startState, world: " ++ show wrld) get

  -- let states = trace ("startState: " ++ show startState) map (\pos -> execState (doInput index1 curPos pos) startState) newPoss
  -- let newStates = trace ("calculate newStates") map (execState performProgram) (trace (show (states)) states)
  -- let a = mergeWorlds newStates
  -- return $ trace (show a) a
  curPos <- gets currPos
  wrld <- gets world
  let newPos' = map (\dir -> addPos curPos (getDirection dir)) allDirs
  let newPoss = filter (flip Map.notMember wrld) newPos'
  startState <- get

  let states = map (\pos -> execState (doInput index1 curPos pos) startState) newPoss
  let newStates = map (execState performProgram) (trace (show (states)) states)
  let a = mergeWorlds newStates
  return a

doInput :: Integer -> Position -> Position -> State Env ()
doInput index1 curPos pos = do
        let inp = trace ("do input") toInteger $ getDir curPos pos
        let input = trace (show inp) inp
        updatePrg (frI index1) inp
        setNewPos $ frI input
        incrementIndex 2
        -- get


mergeWorlds :: [Env] -> World
mergeWorlds envs = Map.unions worlds
  where
    worlds = map world envs

performProgram :: State Env World
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
        -- input <- getInput
        -- updatePrg (frI index1) input
        -- incrementIndex 2
        -- let stat = get
        -- get >>= \stat -> trace (show stat) $ mapelido index1
        trace ("recurse, index1: " ++ show index1) mapelido index1
      [_,4] -> do
        let output = trace (show v1) v1
        newPos' <- gets newPos
        world' <- gets world
        let newWorld = Map.insert newPos' output world'
        case output of
          0 -> return newWorld
          1 -> do
            modify $ \s -> s{world = newWorld}
            setCurrPos newPos'
            incrementIndex 2
            trace ("good") performProgram
          2 -> return newWorld
        -- saveOutput v1
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
        trace ("this should never happen") gets world

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
