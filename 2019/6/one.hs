module Main(main) where

import           Data.List       (foldl')
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

-- map of parents
buildMap :: [String] -> Map String String
buildMap = M.fromList . map parseEdge
 where
  parseEdge [a,b,c,')',x,y,z] = ([x,y,z], [a,b,c])

valueOf :: (Ord a, Num b, Enum b)
        => Map a b -> Map a a -> a -> (b, Map a b)
valueOf cache parMap x
  | x `M.member` cache = (cache ! x, cache)
  | x `M.notMember` parMap = (0, M.insert x 0 cache)
  | otherwise = (found', M.insert x found' cache')
 where
  (found, cache') = valueOf cache parMap (parMap ! x)
  found' = succ found

summarize :: (Ord a, Num b, Enum b)
          => Map a a -> (b, Map a b)
summarize parMap = foldl' build (0, M.empty) $ M.keys parMap
 where
  build (n, cache) k = (n+v, cache')
   where
    (v, cache') = valueOf cache parMap k

main :: IO ()
main = do
  input <- buildMap . lines <$> readFile "input"
  print . fst $ summarize input
