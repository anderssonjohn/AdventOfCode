import Data.List
import Data.Char

----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let passportChecks = map advancedPassports (formatTuples input)
  putStrLn $ show $ length $ filter (True ==) passportChecks


advancedPassports :: [(String, String)] -> Bool
advancedPassports tups = byr && iyr && eyr && hgt && hcl && ecl && pid
  where
    byr = case lookup "byr" tups of
      Nothing -> False
      Just x -> let a = read x in a >= 1920 && a <= 2002
    iyr = case lookup "iyr" tups of
      Nothing -> False
      Just x -> let a = read x in a >= 2010 && a <= 2020
    eyr = case lookup "eyr" tups of
      Nothing -> False
      Just x -> let a = read x in a >= 2020 && a <= 2030
    hgt = case lookup "hgt" tups of
      Nothing -> False
      Just x -> hgt' (length x) x
    hgt' :: Int -> String -> Bool
    hgt' len a
        | len == 5 = let (val, cm) = (splitAt 3 a) in
            (read val) >= 150 && (read val) <= 193 && cm == "cm"
        | len == 4 = let (val, inch) = (splitAt 2 a) in
            (read val) >= 59 && (read val) <= 76 && inch == "in"
        | otherwise = False
    hcl = case lookup "hcl" tups of
      Nothing -> False
      Just x -> (length x) == 7 && (head x) == '#' && and (map hcl' (tail x))
    hcl' :: Char -> Bool
    hcl' char = char `elem` "abcdef0123456789"
    ecl = case lookup "ecl" tups of
      Nothing -> False
      Just x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pid = case lookup "pid" tups of
      Nothing -> False
      Just x -> (length x) == 9 && and (map isDigit x)

formatTuples :: [String] -> [[(String, String)]]
formatTuples [] = []
formatTuples (x:xs) = (map split $ words x):(formatTuples xs)
  where
    split str = splitAt 3 (filter (':' /=) str)
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let passportChecks = map checkPassport input
  putStrLn $ show $ length $ filter (True ==) passportChecks


checkPassport :: String -> Bool
checkPassport str = and contains
  where
    contain content = isInfixOf content str
    contains = map contain ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = filter (not . null) $ fun $ lines input
  where
    fun (x:y:xs) | not (null x) && not (null y) = fun ((x ++ " " ++ y):xs)
                 | null y = x:y:(fun xs)
    fun (x:xs) = [x]
    fun [] = []

type Type = String
type TYPE = [Type]
