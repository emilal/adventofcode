module Main(main) where


read' :: String -> Int
read' ('+':xs) = read xs
read' xs = read xs


main :: IO ()
main = print =<< sum . map read' . lines <$> readFile "input"
