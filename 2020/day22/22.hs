import qualified Data.Set as Set
----------- Code for part two ------------------
two :: IO ()
two = do
  let (_, answer) = playR Set.empty player1 player2
  putStrLn $ show $ sum $ zipWith (*) [50,49..1] answer

playR :: Set.Set ([Int], [Int]) -> [Int] -> [Int] -> (Int, [Int])
playR set a@(c1:h1) b@(c2:h2)
  | Set.member (a, b) set = (1, a)
  | length h1 >= c1 && length h2 >= c2 =
    let (winner, _) = playNew (take c1 h1) (take c2 h2) in
      if winner == 1
      then playNew (h1 ++ [c1, c2]) h2
      else playNew h1 (h2 ++ [c2, c1])
  | otherwise = if c1 > c2
                then playNew (h1 ++ [c1, c2]) h2
                else playNew h1 (h2 ++ [c2, c1])
  where
    newSet = Set.insert (a, b) set
    playNew = playR newSet
playR _ [] h2 = (2, h2)
playR _ h1 [] = (1, h1)
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  let winningHand = play player1 player2
  putStrLn $ show $ sum $ zipWith (*) [50,49..1] winningHand


play :: [Int] -> [Int] -> [Int]
play (c1:h1) (c2:h2) = if c1 > c2 then play (h1 ++ [c1, c2]) h2
  else play h1 (h2 ++ [c2, c1])
play [] h2 = h2
play h1 [] = h1
----------- Code for part one ------------------


----------- Input ----------------------------
player1 = [
  14, 6, 21, 10, 1, 33, 7, 13, 25, 8, 17, 11, 28, 27, 50, 2,
  35, 49, 19, 46, 3, 38, 23, 5, 43
  ]
player2 = [
  18, 9, 12, 39, 48, 24, 32, 45, 47, 41, 40, 15, 22,
  36, 30, 26, 42, 34, 20, 16, 4, 31, 37, 44, 29
  ]
