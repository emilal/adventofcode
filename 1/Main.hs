module Main(main) where

fuel :: (Integral a) => a -> a
fuel = subtract 2 . flip div 3

main :: IO ()
main = do
  masses <- lines <$> readFile "input"
  let
    fuel_reqs = map (fuel . read) masses
  print $ sum fuel_reqs
