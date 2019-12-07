module Main(main) where

import           Data.List       (foldl')
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

-- map of parents
buildMap :: [String] -> Map String String
buildMap = M.fromList . map parseEdge
 where
  parseEdge [a,b,c,')',x,y,z] = ([x,y,z], [a,b,c])

trace :: (Ord a) => a -> Map a a -> [a]
trace edge parMap
  | edge `M.notMember` parMap = [edge]
  | otherwise = edge : trace (parMap ! edge) parMap


combine :: (Eq a) => [a] -> [a] -> Int
combine as@(a:ar) bs@(b:br)
  | a == b = combine ar br
  | otherwise = length ar + length br

main :: IO ()
main = do
  input <- buildMap . lines <$> readFile "input"
  let
    you = trace "YOU" input
    san = trace "SAN" input
  print $ combine (reverse you) (reverse san)
