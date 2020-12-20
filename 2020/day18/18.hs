import Grammar.Abs
import Grammar.Lex
import Grammar.Par
import Grammar.ErrM
----------- Code for part two ------------------
two :: IO ()
two = do
  (Program input) <- readInput
  let answer = sum $ map evalExp input
  putStrLn $ show answer
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  (Program input) <- readInput
  let answer = sum $ map evalExp input
  putStrLn $ show answer
----------- Code for part one ------------------

evalExp :: Exp -> Integer
evalExp (EInt int) = int
evalExp (EAdd e1 e2) = evalExp e1 + evalExp e2
evalExp (EMul e1 e2) = evalExp e1 * evalExp e2

----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = check input

type TYPE = Prg

check :: String -> TYPE
check s = case pPrg (myLexer s) of
    Bad err  -> error "It did not work"
    Ok  tree -> tree