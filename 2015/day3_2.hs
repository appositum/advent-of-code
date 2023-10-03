import Data.Set (fromList, union)

main :: IO ()
main = readFile "inputs/day3.txt" >>= print . solve

solve :: String -> Int
solve str =
    let santa = fst <$> split str
        robot = snd <$> split str
        scan  = fromList . scanl move (0, 0)
    in length $ scan santa `union` scan robot

-- [(santa_moves, robot_moves)]
split :: [a] -> [(a, a)]
split [] = []
split (x:y:ys) = (x, y) : split ys
split (x:xs) = split xs

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '>' = (succ x, y)
move (x, y) '^' = (x, succ y)
move (x, y) '<' = (pred x, y)
move (x, y) 'v' = (x, pred y)
move p _ = p
