import Data.Maybe (fromJust)
import qualified Data.Map as M

main :: IO ()
main = do
    input <- parse . lines <$> readFile "inputs/day01.txt"
    let part1 = foldl (\acc f -> f acc) 0 input
    let part2 = appearTwice $ scanl (\acc f -> f acc) 0 $ cycle input
    putStrLn $ "part 1: " ++ show part1
    putStrLn $ "part 2: " ++ show (fromJust part2)


parse :: [String] -> [Int -> Int]
parse = map parse' where
    parse' :: String -> Int -> Int
    parse' ('-':n) = subtract $ read n
    parse' ('+':n) = (+read n)

appearTwice :: [Int] -> Maybe Int
appearTwice = appearTwice' M.empty where
    appearTwice' :: M.Map Int Int -> [Int] -> Maybe Int
    appearTwice' m [] = Nothing
    appearTwice' m (f:freqs) =
        case M.lookup f m of
            Nothing -> appearTwice' (M.insert f f m) freqs
            _ -> Just f
