module Main(main) where

import           Data.Foldable   (foldl')
import           Data.IntSet     (IntSet)
import qualified Data.IntSet     as I
import           Data.List       (scanl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Util            (isSingleton, split)

type Coord = (Int, Int)


-- Self-crossing doesn't count, and we're just interested in the first time we
-- visit a coordinate
-- fromListWith can't do Int -> [Int] so we need to start with a singleton list
-- Later we'll get a two-element list if there's an intersection
buildMap :: [(Coord, [Int])] -> Map Coord [Int]
buildMap = M.fromListWith (flip const)


-- Could probably do this as a concat.fold with an unfolding function but let's
-- just chug along with a pair of functions
buildCoords :: [String] -> Map Coord [Int]
buildCoords = buildMap . traverse (0,0) 0
 where
  -- this takes an instruction like R10 and translates it into 10 steps of
  -- (succ, id)
  traverse :: Coord -> Int -> [String] -> [(Coord, [Int])]
  traverse _ _ [] = []
  traverse coords steps ((dir:dist):orders) = unroll coords steps funs dist' orders
   where
    dist' = read dist
    funs = case dir of
                 'U' -> (succ, id)
                 'D' -> (pred, id)
                 'R' -> (id, succ)
                 'L' -> (id, pred)
  -- each step of this generates one ((x, y), [steps_taken])
  unroll :: Coord -> Int -> (Int -> Int, Int -> Int) -> Int -> [String] -> [(Coord, [Int])]
  unroll coords steps _ 0 orders = traverse coords steps orders
  unroll (x0, y0) steps funs@(fx, fy) dist orders = (coord, [steps']) : unroll coord steps' funs (pred dist) orders
   where
    steps' = succ steps
    coord = (fx x0, fy y0)


-- filter map where there's just one step value
findIntersections :: Map Coord [Int] -> Map Coord [Int]
findIntersections = M.filter (not . isSingleton)


findClosestIntersection :: Map Coord [Int] -> Int
findClosestIntersection = minimum . filter (/=0) . map add . M.keys
 where
  add (x, y) = abs x + abs y


findShortestIntersection :: Map Coord [Int] -> Int
findShortestIntersection = minimum . map sum . M.elems


main :: IO ()
main = do
  inputs <- lines <$> readFile "input"
  let
    coordMaps = map (buildCoords . split ',') inputs
    coordMap = foldl' (M.unionWith (++)) M.empty coordMaps
    intersections = findIntersections coordMap
    closestIntersection = findClosestIntersection intersections
    shortestIntersection = findShortestIntersection intersections
  print closestIntersection
  print shortestIntersection
