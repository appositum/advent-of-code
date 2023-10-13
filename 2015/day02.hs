main :: IO ()
main = do
  input <- readFile "inputs/day02.txt"
  putStrLn $ "part 1: " ++ show (solve part1 input)
  putStrLn $ "part 2: " ++ show (solve part2 input)

solve :: ([Int] -> Int) -> String -> Int
solve f = sum . map (f . split) . lines

part1 :: [Int] -> Int
part1 dim@[l, w, h] = (2*l*w + 2*w*h + 2*h*l) + smallestSide dim

part2 :: [Int] -> Int
part2 dim = smallestPerimeter dim + product dim

split :: String -> [Int]
split = map (\s -> read s :: Int) . words . map f
  where f 'x' = ' '
        f x = x

smallestSide :: [Int] -> Int
smallestSide [l, w, h] = minimum [l*w, w*h, h*l]

smallestPerimeter :: [Int] -> Int
smallestPerimeter [l, w, h] = minimum [p1, p2, p3]
  where p1 = 2 * (l + w)
        p2 = 2 * (w + h)
        p3 = 2 * (h + l)
