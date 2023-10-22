import Data.Char (ord)

main :: IO ()
main = do
    input <- head <$> words <$> readFile "inputs/day11.txt"
    let passwd1 = generateNewPassword input
    let passwd2 = generateNewPassword passwd1
    putStrLn $ "part 1: " ++ passwd1 ++ "\npart 2: " ++ passwd2

generateNewPassword :: String -> String
generateNewPassword s = head $ dropWhile (not . abidesRules) newPasswords
  where newPasswords = tail $ iterate incrementString s
  -- tail removes the first element, which is the input string
  -- this way, if the current password is already valid, we skip it

incrementString :: String -> String
incrementString [] = []
incrementString s =
    let lastChar = last s
        lastChar' = incrementChar lastChar
    in if lastChar == 'z'
    then incrementString (init s) ++ [lastChar']
    else init s ++ [lastChar']

incrementChar :: Char -> Char
incrementChar 'z' = 'a'
incrementChar c = succ c

threeStraight :: String -> Bool
threeStraight (a:b:c:rest)
    | ord a == pred (ord b) && ord b == pred (ord c) = True
    | otherwise = threeStraight (b:c:rest)
threeStraight _ = False

bannedLetter :: String -> Bool
bannedLetter = any (`elem` "iol")

-- input -> (found_pair, rest_of_input)
pair :: String -> (String, String)
pair (x:y:ys)
    | x == y = ((x:y:""), ys)
    | otherwise = pair (y:ys)
pair xs = ("", xs)

rule1 :: String -> Bool
rule1 = threeStraight

rule2 :: String -> Bool
rule2 = not . bannedLetter

rule3 :: String -> Bool
rule3 s =
    let (s1, rest1) = pair s
        (s2, rest2) = pair rest1
        notEmpty l = length l > 0
    in s1 /= s2 && notEmpty s1 && notEmpty s2

abidesRules :: String -> Bool
abidesRules s = rule1 s && rule2 s && rule3 s
