import Data.List
----------- Code for part two ------------------
two :: IO ()
two = do
  putStrLn $ show t
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do

  let a = [(x, [x * y | y <- [1..], x * y >= timestamp]) | x <- buses]
  let departures = map (\(line, departures) -> (line, head departures)) a
  let (bus, time) = head $ sortOn snd departures
  putStrLn $ show $ bus * (time - timestamp)

----------- Code for part one ------------------


timestamp = 1001612
buses = [19,41,37,821,13,17,29,463,23]
buses' = [(0, 19),(9, 41),(13, 37),(19, 821),(32, 13),(36, 17),(48, 29),(50, 463),(73, 23)]


{- System of equations
t = 19 * a,
t + 9 = 41 * b,
t + 13 = 37 * c,
t + 19 = 821 * d,
t + 32 = 13 * e,
t + 36 = 17 * f,
t + 48 = 29 * g,
t + 50 = 463 * h,
t + 73 = 23 * i,
-}

-- Credit to wolframalpha
t = 554865447501099
