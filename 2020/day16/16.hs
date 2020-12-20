import Data.List.Split
import Data.List
import Data.Maybe

----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let filtered = filter (all (\x -> lowest <= x && highest >= x)) input
  let transposed = map sort $ transpose filtered
  let bools = map (\line -> map (\fun -> all fun line) funs) transposed
  let indices = map (\ls -> map fst (filter ((== True). snd) (zip [1..] ls))) bools
  let things = foldl (fun (zip [1..] indices)) [] $ [1..20]
  let departures = filter (\x -> snd x <= 6) things
  let answer = product $ map (\x -> myTicket !! (fst x - 1)) departures
  putStrLn $ show answer

fun :: [(Int,[Int])] -> [(Int, Int)] -> Int -> [(Int, Int)]
fun indices intmap index = (lsIndex,newValue):intmap
  where
    (lsIndex, ls) = fromJust $ find (\x -> length (snd x) == index) indices
    [newValue] = ls \\ (map snd intmap)
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  input <- readInput
  let answer = sum $ filter (\x -> lowest > x || highest < x) $ concat input
  putStrLn $ show  answer
----------- Code for part one ------------------


----------- Input ----------------------------
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = (map . map) read $ map (splitOn ",") $ lines input

type Type = [Int]
type TYPE = [Type]

lowest = 25
highest = 974

myTicket :: [Integer]
myTicket = [89,139,79,151,97,67,71,53,59,149,127,131,103,109,137,73,101,83,61,107]
funs = [departureLocation, departureStation, departurePlatform, departureTrack, departureDate, departureTime, arrivalLocation, arrivalStation, arrivalPlatform, arrivalTrack, class', duration, price, route, row, seat, train, type', wagon, zone]
departureLocation x = (44 <= x && x <= 401 ) || ( 415 <= x && x <= 965)
departureStation x = (44 <= x && x <= 221 ) || ( 243 <= x && x <= 953)
departurePlatform x = (29 <= x && x <= 477 ) || ( 484 <= x && x <= 963)
departureTrack x = (43 <= x && x <= 110 ) || ( 126 <= x && x <= 951)
departureDate x = (48 <= x && x <= 572 ) || ( 588 <= x && x <= 965)
departureTime x = (48 <= x && x <= 702 ) || ( 719 <= x && x <= 955)
arrivalLocation x = (35 <= x && x <= 336 ) || ( 358 <= x && x <= 960)
arrivalStation x = (47 <= x && x <= 442 ) || ( 449 <= x && x <= 955)
arrivalPlatform x = (25 <= x && x <= 632 ) || ( 639 <= x && x <= 970)
arrivalTrack x = (34 <= x && x <= 461 ) || ( 472 <= x && x <= 967)
class' x = (41 <= x && x <= 211 ) || ( 217 <= x && x <= 959)
duration x = (29 <= x && x <= 500 ) || ( 519 <= x && x <= 969)
price x = (39 <= x && x <= 423 ) || ( 440 <= x && x <= 969)
route x = (50 <= x && x <= 264 ) || ( 282 <= x && x <= 958)
row x = (50 <= x && x <= 907 ) || ( 920 <= x && x <= 972)
seat x = (27 <= x && x <= 294 ) || ( 315 <= x && x <= 954)
train x = (29 <= x && x <= 813 ) || ( 827 <= x && x <= 962)
type' x = (45 <= x && x <= 531 ) || ( 546 <= x && x <= 956)
wagon x = (29 <= x && x <= 283 ) || ( 292 <= x && x <= 957)
zone x = (45 <= x && x <= 518 ) || ( 525 <= x && x <= 974)
