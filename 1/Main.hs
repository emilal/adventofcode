module Main(main) where


fuel :: (Integral a) => a -> a
fuel = subtract 2 . flip div 3


fuelWithMass :: (Integral a) => a -> a
fuelWithMass startMass = sum $ takeWhile (>0) fuelMasses
 where
  startFuel = fuel startMass
  fuelMasses = startFuel : map fuel fuelMasses


one :: (Integral a, Show a) => [a] -> IO ()
one = print . sum . map fuel


two :: (Integral a, Show a) => [a] ->IO ()
two = print . sum . map fuelWithMass


main :: IO ()
main = do
  masses <- map read . lines <$> readFile "input"
  one masses
  two masses
