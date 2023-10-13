import Prelude hiding (floor)

main :: IO ()
main = do
    input <- readFile "inputs/day01.txt"
    putStrLn $ "part 1: " ++ show (part1 input)
    putStrLn $ "part 2: " ++ show (part2 input)

part1 :: String -> Int
part1 = floor 0

part2 :: String -> Int
part2 = basement 0 0

floor :: Int -> String -> Int
floor n [] = n
floor n ('(':xs) = floor (n+1) xs
floor n (')':xs) = floor (n-1) xs
floor n (_:xs) = floor n xs

basement :: Int -> Int -> String -> Int
basement floor pos [] = pos
basement (-1) pos _  = pos
basement floor pos ('(':xs) = basement (floor+1) (succ pos) xs
basement floor pos (')':xs) = basement (floor-1) (succ pos) xs
basement floor pos (_:xs) = basement floor pos xs
