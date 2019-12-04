module Main(main) where

import           Data.IntSet (IntSet)
import qualified Data.IntSet as I


read' :: String -> Int
read' ('+':xs) = read xs
read' xs = read xs


scan :: IntSet -> Int -> [Int] -> Maybe Int
scan _ _ [] = Nothing
scan s n (x:xs)
  | n' `I.member` s = Just n'
  | otherwise = scan s' n' xs
 where
  n' = n + x
  s' = I.insert n' s


main :: IO ()
main = print =<< scan (I.singleton 0) 0 . concat . repeat . map read' . lines <$> readFile "input"
