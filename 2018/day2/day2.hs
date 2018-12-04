import Data.List

main = interact (show . checksum . lines)

checksum :: [String] -> Int
checksum lines =
    (length (filter hasDoubleLetter letterCounts)) * (length (filter hasTripleLetter letterCounts))
    where letterCounts = map countLetters lines

countLetters :: String -> [Int]
countLetters = (map length) . group . sort

hasDoubleLetter :: [Int] -> Bool
hasDoubleLetter = elem 2

hasTripleLetter :: [Int] -> Bool
hasTripleLetter = elem 3
