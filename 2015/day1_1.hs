main :: IO ()
main = readFile "inputs/day1.txt" >>= print . solve

solve :: String -> Int
solve = solve' 0

solve' :: Int -> String -> Int
solve' n [] = n
solve' n ('(':xs) = solve' (n+1) xs
solve' n (')':xs) = solve' (n-1) xs
