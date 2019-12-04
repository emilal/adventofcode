module Main(main) where

import           Data.Char (digitToInt)
import           Data.List (group)

increasing :: [Int] -> Bool
increasing xs = and . zipWith (<=) xs $ tail xs


doubles :: [Int] -> Bool
doubles = any ((==2) . length) . group


digitize :: Int -> [Int]
digitize = map digitToInt . show


check :: Int -> Bool
check x = increasing x' && doubles x'
 where
  x' = digitize x

main = print . length $ filter check [353096..843212]
