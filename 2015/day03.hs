import Data.Set (fromList, union)

main :: IO ()
main = do
  input <- readFile "inputs/day03.txt"
  putStrLn $ "part 1: " ++ show (part1 input)
  putStrLn $ "part 2: " ++ show (part2 input)

part1 :: String -> Int
part1 = length . fromList . scanl move (0, 0)

part2 :: String -> Int
part2 str =
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
