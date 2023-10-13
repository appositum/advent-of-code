import Data.List (isInfixOf)

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/day05.txt"
  putStrLn $ "part 1: " ++ show (part1 input)
  putStrLn $ "part 2: " ++ show (part2 input)

part1 :: [String] -> Int
part1 = length . filter f
  where f s = threeVowels s && twiceInAroll s && (not . bannedString) s

part2 :: [String] -> Int
part2 = length . filter f
  where f s = pair s && between s

threeVowels :: String -> Bool
threeVowels = (>2) . length . filter (`elem` "aeiou")

twiceInAroll :: String -> Bool
twiceInAroll (x:y:ys)
    | x == y = True
    | otherwise = twiceInAroll (y:ys)
twiceInAroll _ = False

bannedString :: String -> Bool
bannedString s = any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

pair :: String -> Bool
pair (x:y:ys)
    | [x, y] `isInfixOf` ys = True
    | otherwise = pair (y:ys)
pair _ = False

between :: String -> Bool
between (x:y:z:zs)
    | x == z = True
    | otherwise = between (y:z:zs)
between _ = False
