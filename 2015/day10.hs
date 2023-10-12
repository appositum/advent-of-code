import Data.Char (chr)
import Data.List (group)

main :: IO ()
main = do
    input <- readFile "inputs/day10.txt"
    let solve n = length . applyNtimes lookAndSay n . takeWhile (/='\n')
    putStrLn $ "part 1: " ++ show (solve 40 input)
    putStrLn $ "part 2: " ++ show (solve 50 input)

applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f n a = iterate f a !! n

lookAndSay :: String -> String
lookAndSay =
    foldr (\(a, b) acc -> b:a:acc) []
    . map (\s -> (head s, toChar (length s)))
    . group

toChar :: Int -> Char
toChar n = chr (48 + n)
