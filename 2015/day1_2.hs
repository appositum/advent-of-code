main :: IO ()
main = readFile "inputs/day1.txt" >>= print . solve

solve :: String -> Int
solve = solve' 0 0

solve' :: Int -> Int -> String -> Int
solve' floor pos [] = pos
solve' (-1) pos _  = pos
solve' floor pos ('(':xs) = solve' (floor+1) (succ pos) xs
solve' floor pos (')':xs) = solve' (floor-1) (succ pos) xs