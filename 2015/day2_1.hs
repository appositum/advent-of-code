main :: IO ()
main = readFile "inputs/day2.txt" >>= print . solve

solve :: String -> Int
solve = sum . map (solve' . split) . lines

solve' :: [Int] -> Int
solve' dim@[l, w, h] = (2*l*w + 2*w*h + 2*h*l) + lowest dim

lowest :: [Int] -> Int
lowest [l, w, h] = minimum [l*w, w*h, h*l]

split :: String -> [Int]
split = map (\s -> read s :: Int) . words . map f
  where f 'x' = ' '
        f x = x
