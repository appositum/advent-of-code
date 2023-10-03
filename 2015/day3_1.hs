import qualified Data.Set as S

type Point = (Int, Int)
type Set = S.Set Point

main :: IO ()
main = readFile "inputs/day3.txt" >>= print . solve

solve :: String -> Int
solve = length . walk (0, 0) S.empty

move :: Char -> Point -> Point
move '>' (x, y) = (succ x, y)
move '^' (x, y) = (x, succ y)
move '<' (x, y) = (pred x, y)
move 'v' (x, y) = (x, pred y)
move _ p = p

walk :: Point -> Set -> String -> Set
walk _ set "" = set
walk p set (x:xs) = walk newPoint newSet xs
  where newPoint = move x p
        newSet = S.insert p set
