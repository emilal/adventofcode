module Main(main) where

import           Data.Char (digitToInt)

increasing :: [Int] -> Bool
increasing xs = and . zipWith (<=) xs $ tail xs


doubles :: [Int] -> Bool
doubles xs = or . zipWith (==) xs $ tail xs


digitize :: Int -> [Int]
digitize = map digitToInt . show


check :: Int -> Bool
check x = increasing x' && doubles x'
 where
  x' = digitize x

main = print . length $ filter check [353096..843212]
