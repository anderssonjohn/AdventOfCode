import Control.Monad.State
import Data.Set hiding (map, filter)
import Data.List (find)
----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let inputs = map emptyEnv $ replaceOps [] input
  let results = map (execState runProgram) inputs
  -- let (Just result) = find (\env -> not (loop env)) results
  let res = filter (\env -> not (loop env)) results
  -- putStrLn $ show $ acc result
  putStrLn $ show $ map (\env -> acc env) res

replaceOps :: Program -> Program -> [Program]
replaceOps _ [] = []
replaceOps prev ((op, arg):xs) = case op of
                                  "jmp" -> (prev ++ (("nop", arg):xs)): (replaceOps (prev ++ [(op,arg)]) xs)
                                  "nop" -> (prev ++ (("jmp", arg):xs)) : (replaceOps (prev ++ [(op,arg)]) xs)
                                  otherwise -> replaceOps (prev ++ [(op, arg)]) xs
----------- Code for part two ------------------
test :: IO ()
test = do
  input <- readTest "test1.txt"
  let a@(val, env) = runState runProgram (emptyEnv input)
  putStrLn $ show $ a -- acc env
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let (val, env) = runState runProgram (emptyEnv input)
  putStrLn $ show $ acc env

data Env = Env {
  program :: Program,
  index :: Int,
  visited :: Set Int,
  acc :: Int,
  loop :: Bool
} deriving Show

emptyEnv :: Program -> Env
emptyEnv prg = Env {program = prg, index = 0, visited = empty, acc = 0, loop = False}

incrementAcc :: Int -> State Env ()
incrementAcc offset = do
  curr <- gets acc
  modify $ \s -> s{acc = curr + offset}

incrementIndex :: Int -> State Env ()
incrementIndex offset = do
  curr <- gets index
  modify $ \s -> s{index = curr + offset}
incOne :: State Env ()
incOne = incrementIndex 1

checkVisited :: Int -> State Env Bool
checkVisited index = do
  visited <- gets visited
  if index `elem` visited then modify (\s -> s{loop = True}) >> return True
    else do
      modify $ \s -> s{visited = insert index visited}
      return False

runProgram :: State Env ()
runProgram = do
  index <- gets index
  program <- gets program
  visited <- gets visited
  if index >= (length program) then return ()
    else do
  let a@(op, arg) = program !! index
  visitedOp <- checkVisited index
  if visitedOp then return ()
    else do
  case op of
    "nop" -> incOne
    "acc" -> incrementAcc arg >> incOne
    "jmp" -> incrementIndex arg
  runProgram
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO Program
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

readTest :: String -> IO Program
readTest fileName = do
  input <- readFile fileName
  return $ formatInput input

formatInput :: String -> Program
formatInput input = map tup $ lines input
  where
    tup line = let [op, arg] = words line in (op, read $ filter ('+' /=) arg)

type Operation = String
type Argument = Int
type Type = (Operation, Argument)
type Program = [Type]
