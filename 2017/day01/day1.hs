import Data.Char

main = interact $ unlines . map (show . checksum' . parseInput) . lines

parseInput :: String -> [Int]
parseInput = map digitToInt

checksum :: [Int] -> Int
checksum digits =
    let
        pairs = zip digits $ drop 1 $ cycle digits
    in
        (sum . map fst . filter (\(x,y) -> x == y)) pairs

checksum' :: [Int] -> Int
checksum' digits =
    let
        pairs = zip digits $ drop (length digits `quot` 2) $ cycle digits
    in
        (sum . map fst . filter (\(x,y) -> x == y)) pairs

