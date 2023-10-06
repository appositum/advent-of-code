import Data.List (isInfixOf)

main :: IO ()
main = readFile "inputs/day5.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = length . filter f
  where f s = pair s && between s

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
