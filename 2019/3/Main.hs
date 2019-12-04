module Main(main) where

import           Data.Foldable   (foldl')
import           Data.List       (scanl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Util            (split)

type Coord = (Int, Int)


-- Self-crossing doesn't count
buildMap :: [Coord] -> Map Coord Int
buildMap = M.fromList . flip zip (repeat 1)


buildCoords :: [String] -> Map Coord Int
buildCoords = buildMap . concat . scanl' chunk [(0,0)]
 where
  chunk :: [Coord] -> String -> [Coord]
  chunk coords (dir:dist) = unroll origin funs dist'
   where
    origin = last coords
    dist' = read dist
    funs = case dir of
                 'U' -> (succ, id)
                 'D' -> (pred, id)
                 'R' -> (id, succ)
                 'L' -> (id, pred)
  unroll :: Coord -> (Int -> Int, Int -> Int) -> Int -> [Coord]
  unroll _ _ 0 = []
  unroll (x0, y0) funs@(fx, fy) dist = coord : unroll coord funs (pred dist)
   where
    coord = (fx x0, fy y0)


-- filter map where value > 1
findIntersections :: Map Coord Int -> Map Coord Int
findIntersections = M.filter (>1)


findClosestIntersection :: Map Coord Int -> Int
findClosestIntersection = minimum . filter (/=0) . map add . M.keys
 where
  add (x, y) = abs x + abs y

main :: IO ()
main = do
  inputs <- lines <$> readFile "input"
  let
    coordMaps = map (buildCoords . split ',') inputs
    coordMap = foldl' (M.unionWith (+)) M.empty coordMaps
    intersections = findIntersections coordMap
    closestIntersection = findClosestIntersection intersections
  print closestIntersection
