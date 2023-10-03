main :: IO ()
main = readFile "inputs/day2.txt" >>= print . solve

solve :: String -> Int
solve = sum . map (solve' . split) . lines

solve' :: [Int] -> Int
solve' dim = smallest_perimeter dim + volume dim
  where volume = foldr (*) 1

smallest_perimeter :: [Int] -> Int
smallest_perimeter [l, w, h] = minimum [p1, p2, p3]
  where p1 = 2 * (l + w)
        p2 = 2 * (w + h)
        p3 = 2 * (h + l)

split :: String -> [Int]
split = map (\s -> read s :: Int) . words . map f
  where f 'x' = ' '
        f x = x
