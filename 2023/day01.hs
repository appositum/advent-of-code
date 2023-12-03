import Data.Char

main :: IO ()
main = do
    input <- lines <$> readFile "inputs/day01.txt"
    putStrLn $ "part 1: " ++ (show . sum . map calibrationValuePt1) input
    putStrLn $ "part 2: " ++ (show . sum . map calibrationValuePt2) input


calibrationValuePt1 :: String -> Int
calibrationValuePt1 line = read $ head digits : last digits : ""
  where digits = filter isDigit line

calibrationValuePt2 :: String -> Int
calibrationValuePt2 line =
    let parsedFirst@(Just first, _) = parseFirst line
        (Just last, _) = parseLast parsedFirst
    in read $ first : last : ""

-- input -> (result, rest)
parseFirst :: String -> (Maybe Char, String)
parseFirst "" = (Nothing, "")
parseFirst line@(c:rest)
    | isDigit c = (Just c, rest)
    | otherwise =
        case line of
            -- include last character on rest, for cases like `oneightr`.
            -- it should be (8, "r") instead of (1, "ightr").
            ('o':'n':cs@('e':_)) -> (Just '1', cs)
            ('t':'w':cs@('o':_)) -> (Just '2', cs)
            ('t':'h':'r':'e':cs@('e':_)) -> (Just '3', cs)
            ('f':'o':'u':cs@('r':_)) -> (Just '4', cs)
            ('f':'i':'v':cs@('e':_)) -> (Just '5', cs)
            ('s':'i':cs@('x':_)) -> (Just '6', cs)
            ('s':'e':'v':'e':cs@('n':_)) -> (Just '7', cs)
            ('e':'i':'g':'h':cs@('t':_)) -> (Just '8', cs)
            ('n':'i':'n':cs@('e':_)) -> (Just '9', cs)
            _ -> parseFirst rest

parseLast :: (Maybe Char, String) -> (Maybe Char, String)
parseLast initial =
    let (Just first, rest) = initial
    in case parseFirst rest of
        (Nothing, rest') -> (Just first, rest')
        initial'@(Just second, rest') -> parseLast initial'
