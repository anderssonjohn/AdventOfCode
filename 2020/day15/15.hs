import Data.List hiding (lookup, insert)
import Data.IntMap hiding (foldl')

import Prelude hiding (lookup)
main = two
----------- Code for part two ------------------
two :: IO ()
two = do
  let input = fromList $ zip startingInput [1..]
  let answer = fun' 30000000 input
  putStrLn $ show $ answer
----------- Code for part two ------------------
----------- Code for part one ------------------
one :: IO ()
one = do
  let input = fromList $ zip startingInput [1..]
  let answer = fun' 2020 input
  putStrLn $ show $ answer

fun' :: Int -> IntMap Int -> Int
fun' amount ls = snd $ foldl' updateMap (ls, 0) [8..(amount - 1)]
  where
    updateMap :: (IntMap Int, Int) -> Int -> (IntMap Int, Int)
    updateMap (ls, curr) index = case lookup curr ls of
      Nothing -> (insert curr index ls, 0)
      Just oldIndex -> (insert curr index ls, (index - oldIndex))
----------- Code for part one ------------------


----------- Input ----------------------------

startingInput = [0,6,1,7,2,19,20]
