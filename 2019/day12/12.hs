import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = b

input :: [Planet]
input = [ (5, -1, 5), (0, -14, 2), (16, 4, 0), (18, 1, 16) ]
t1 = [(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)]
t2 = [( -8, -10, 0 ), ( 5, 5, 10 ), ( 2, -7, 3 ), ( 9, -8, -3 )]

universify :: [Planet] -> Universe
universify planets = zip planets $ repeat (0,0,0)

a :: IO ()
a = do
  mainA $ universify input
b :: IO ()
b = do
  mainB input

test1 :: IO ()
test1 = do
  mainA $ universify t1

test2 :: IO ()
test2 = do
  mainB t2
test3 :: IO ()
test3 = do
  mainB t1


type Universe = [(Planet, Velocity)]
type Planet = (Integer, Integer, Integer)
type Velocity = (Integer, Integer, Integer)
----------- Code for part b ------------------
type NiceType = Set ([PosVect])
type PosVect = (Integer,Integer)

mainB :: [Planet] -> IO ()
mainB universe = do
  let (x,y,z) = splitAxes universe
  let xRepeat = findRepeat x
  let yRepeat = findRepeat y
  let zRepeat = findRepeat z
  print xRepeat
  print yRepeat
  print zRepeat



findRepeat :: NiceType -> Int
findRepeat set = simulateAxes first set
  where
    first = head $ Set.toList set

simulateAxes :: [PosVect] -> NiceType -> Int
simulateAxes curr set = if Set.member new set then Set.size set else simulateAxes new set' -- set'
  where
    new = calculateAxes curr
    set' = Set.insert new set
    size = Set.size set
    size' = Set.size set'

calculateAxes :: [PosVect] -> [PosVect]
calculateAxes [(c1,v1),(c2,v2),(c3,v3),(c4,v4)] = [(c1',v1'),(c2',v2'),(c3',v3'),(c4',v4')]
  where
    v1' = v1 + (g' c1 c2) + (g' c1 c3) + (g' c1 c4)
    v2' = v2 + (g' c2 c1) + (g' c2 c3) + (g' c2 c4)
    v3' = v3 + (g' c3 c1) + (g' c3 c2) + (g' c3 c4)
    v4' = v4 + (g' c4 c1) + (g' c4 c2) + (g' c4 c3)
    c1' = c1 + v1'
    c2' = c2 + v2'
    c3' = c3 + v3'
    c4' = c4 + v4'

splitAxes :: [Planet] -> (NiceType, NiceType, NiceType)
splitAxes [(x1,y1,z1),(x2,y2,z2),(x3,y3,z3),(x4,y4,z4)] = (xSet,ySet,zSet)
  where
    xSet = Set.fromList [[(x1,0),(x2,0),(x3,0),(x4,0)]]
    ySet = Set.fromList [[(y1,0),(y2,0),(y3,0),(y4,0)]]
    zSet = Set.fromList [[(z1,0),(z2,0),(z3,0),(z4,0)]]
----------- Code for part b ------------------
----------- Code for part a ------------------
mainA :: Universe -> IO ()
mainA universe = do
  let univ' = foldl' fn universe [1..1000]
  print $ calculateEnergy univ'

fn :: Universe -> Int -> Universe
fn [(p1,v1),(p2,v2),(p3,v3),(p4,v4)] _ = [(p1',v1'),(p2',v2'),(p3',v3'),(p4',v4')]
  where
    v1' = add (add (gravity p1 p2) (gravity p1 p3)) (add (gravity p1 p4) v1)
    v2' = add (add (gravity p2 p1) (gravity p2 p3)) (add (gravity p2 p4) v2)
    v3' = add (add (gravity p3 p1) (gravity p3 p2)) (add (gravity p3 p4) v3)
    v4' = add (add (gravity p4 p1) (gravity p4 p2)) (add (gravity p4 p3) v4)
    p1' = add p1 v1'
    p2' = add p2 v2'
    p3' = add p3 v3'
    p4' = add p4 v4'

calculateEnergy :: Universe -> Integer
calculateEnergy univ = sum $ map (\(p,v) -> ( absTup p ) * ( absTup v )) univ

absTup :: Planet -> Integer
absTup (x,y,z) = ( abs x ) + ( abs y ) + ( abs z )

add :: Velocity -> Velocity -> Velocity
add (x,y,z) (x1,y1,z1) = (x + x1, y + y1, z + z1)

gravity :: Planet -> Planet -> Velocity
gravity (x,y,z) (x1,y1,z1) = (g' x x1, g' y y1, g' z z1)

g' :: Integer -> Integer -> Integer
g' a b
  | a < b = 1
  | a > b = -1
  | a == b = 0


----------- Code for part a ------------------


