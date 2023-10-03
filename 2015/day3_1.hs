import Data.Set (fromList)

main :: IO ()
main = readFile "inputs/day3.txt" >>= print . solve

solve :: String -> Int
solve = length . fromList . scanl move (0, 0)
  where
    move (x, y) '>' = (succ x, y)
    move (x, y) '^' = (x, succ y)
    move (x, y) '<' = (pred x, y)
    move (x, y) 'v' = (x, pred y)
    move p _ = p
