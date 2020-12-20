
import           Control.Monad (void)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import qualified Text.Parsec.Token as L
import           Text.Parsec.String (Parser)
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
-- -- TODO: Implement this, but in fun instead
-- evaluateParens :: String -> String
-- evaluateParens ('(':xs) = if '(' `elem` xs then evaluateParens xs else doStuff xs
--   where
--     doStuff string = let (inParens, (_:afterParens)) = span (')' /= ) string in
-- evaluateParens (')':xs) =
-- evaluateParens (x:xs) =

-- evaluateLine :: String -> Int
-- evaluateLine ('(':xs++')'++ys) = evaluateLine xs
-- evaluateLine (x:'*':xs) = x * evaluateLine xs
-- evaluateLine (x:'+':xs) = x + evaluateLine xs

----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput a = [(Const 1)]
-- formatInput :: String -> TYPE
-- formatInput input = map fun $ lines $ filter (' ' /= ) input
--   where
--     fun :: String -> Exp
--     fun ('(':xs)   = let (e, rest) = parens xs in case rest of
--       ('+':xs) -> Add e $ fun xs
--       ('*':xs) -> Mult e $ fun xs
--       []       -> e
--     fun (x:'*':xs) = Mult (Const (read x)) $ fun xs
--     fun (x:'+':xs) = Add (Const (read x)) $ fun xs
--     parens :: String -> (Exp, String)
--     parens xs = if '(' `elem` xs then evaluateParens xs else doStuff xs
--     doStuff string = let (inParens, (_:afterParens)) = span (')' /= ) string in (fun inParens, afterParens)
-- (7 * 5 + (7 + 7 + 8) * 3 * (7 * 5)) + 3
type Type = Exp
type TYPE = [Type]

data Exp = Add Exp Exp | Parens Exp | Mul Exp Exp | Const Int

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = space (void spaceChar) -- lineComment blockComment
  -- where lineComment  = L.skipLineComment "//"
        -- blockComment = L.skipBlockComment "/*" "*/"

-- lexeme = L.lexeme -- sc
-- symbol = L.symbol -- sc

parens :: Parser Exp -> Parser Exp
parens = between (symbol "(") (symbol ")")

term =  parens expr
    <|> Const <$> natural

table = [ [binary "*" Mul AssocLeft, binary "+" Add AssocLeft ]
        ]

binary  name fun assoc = Infix   (do { reservedOp name; return fun }) assoc

expr :: Parser Exp
expr = buildExpressionParser table term
