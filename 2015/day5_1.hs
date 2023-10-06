import Data.List (isInfixOf)

main :: IO ()
main = readFile "inputs/day5.txt" >>= print . solve . lines

solve :: [String] -> Int
solve = length . filter f
  where f s = threeVowels s && twiceInAroll s && (not . bannedString) s

threeVowels :: String -> Bool
threeVowels = (>2) . length . filter (`elem` "aeiou")

twiceInAroll :: String -> Bool
twiceInAroll (x:y:ys)
    | x == y = True
    | otherwise = twiceInAroll (y:ys)
twiceInAroll _ = False

bannedString :: String -> Bool
bannedString s = any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]
