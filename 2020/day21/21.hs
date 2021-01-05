import Data.Map hiding (drop, take, map, foldl, foldr, (\\))
import qualified Data.Map as Map
import Data.List.Split
import Prelude hiding (lookup)
import Data.Set hiding (drop, take, map, foldl, empty, foldr, (\\))
import qualified Data.Set as Set
import qualified Data.List as List
----------- Code for part two ------------------
two :: IO ()
two = do
  input <- readInput
  let allergenIngredients = Map.map findAllergenIngredient input

  let allergens = List.sortOn (length . snd) $ Map.toList allergenIngredients
  putStrLn $ show $ allergens

dairy = "dpkvsdk"
eggs = "xmmpt"
fish = "cxjqxbt"
nuts = "drbq"
peanuts = "zmzq"
sesame = "mnrjrf"
shellfish = "kjgl"
wheat = "rkcpxs"
----------- Code for part two ------------------
----------- Code for part one ------------------
test1 = do
  input <- test1input
  let allergenIngredients = Map.map findAllergenIngredient input
  let allergens = List.nub $ concat $ Map.foldr (:) [] allergenIngredients
  let ingredients = concat $ List.nub $ concat $ Map.foldr (:) [] input

  putStrLn $ show $ List.filter (\x -> not (x `elem` allergens)) ingredients
one :: IO ()
one = do
  input <- readInput
  let allergenIngredients = Map.map findAllergenIngredient input
  let allergens = List.nub $ concat $ Map.foldr (:) [] allergenIngredients
  let ingredients = concat $ List.nub $ concat $ Map.foldr (:) [] input

  putStrLn $ show $ length $ List.filter (\x -> not (x `elem` allergens)) ingredients

findAllergenIngredient :: IngredientList -> Ingredients
findAllergenIngredient ingredientLists = ingredient
  where
    ingredient = Set.toList $ foldl (\set ls -> Set.intersection set (Set.fromList ls)) (Set.fromList (head ingredientLists)) $ tail ingredientLists
----------- Code for part one ------------------


----------- Input ----------------------------
test1input = do
  input <- readFile "test1.txt"
  return $ formatInput input
readInput :: IO TYPE
readInput = do
  input <- readFile "input.txt"
  return $ formatInput input

formatInput :: String -> TYPE
formatInput input = foldl insertFoods empty $ lines input
  where
    insertFoods :: TYPE -> String -> TYPE
    insertFoods allergenMap line = let allergens = findAllergens line in
      foldl (\allMap allergen -> insertWith (++) allergen [(findIngredients line)] allMap) allergenMap allergens
    findIngredients :: String -> [String]
    findIngredients food = words $ takeWhile (/= '(') food
    findAllergens :: String -> [String]
    findAllergens food =  splitOn ", " $ (\x -> take (length x - 1) x) $ drop 10 $ dropWhile (/= '(') food

type Allergen = String
type Ingredient = String
type Ingredients = [Ingredient]
type IngredientList = [Ingredients]
type TYPE = Map Allergen IngredientList
