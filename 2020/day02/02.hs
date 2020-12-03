{-# LANGUAGE FlexibleContexts #-}
import qualified Text.Parsec as Parsec
import Control.Monad.Identity (Identity)

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

test2 :: IO ()
test2 = do
  input <- readInput "test2.txt"
  mainB input


----------- Code for part b ------------------
mainB :: Input -> IO ()
mainB input = do
  let count = length [x | x <- input, funB x]
  putStrLn $ show count


funB :: Type -> Bool
funB (min, max, char, password)
  | first && second = False
  | first || second = True
  | otherwise = False
  where
    first = password !! (min - 1) == char
    second = password !! (max - 1) == char
----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: Input -> IO ()
mainA input = do
  let count = length [x | x <- input, fun x]
  putStrLn $ show count


fun :: Type -> Bool
fun (min, max, char, password) = if count >= min && count <= max then
  True
  else False
  where
    count = length $ filter (char ==) password
----------- Code for part a ------------------


----------- Input ----------------------------
readInput :: String -> IO Input
readInput filePath = do
  input <- readFile filePath
  return $ formatInput input

formatInput :: String -> Input
formatInput inp = map fun (lines inp)
  where
    fun line = case parse doTheParse line of
                 Right a -> a
                 Left e -> error "Invalid input"


type Input = [Type]
type Type = (Int, Int, Char, String)

parse rule text = Parsec.parse rule "unused" text

doTheParse :: Parsec.Parsec String () Type
doTheParse = do
  min <- Parsec.manyTill Parsec.digit (Parsec.char '-') >>= return . read
  max <- Parsec.manyTill Parsec.digit (Parsec.char ' ') >>= return . read
  char <- Parsec.letter
  Parsec.char ':'
  Parsec.space
  password <- Parsec.many Parsec.letter
  return (min, max, char, password)


inputFile = "input.txt"
