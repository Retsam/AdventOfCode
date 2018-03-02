-- main = interact $ show . sum . map (checksum . parseInput) . lines
main = interact $ show . sum . map (checksum' . parseInput) . lines

-- PART A

parseInput :: String -> [Int]
parseInput = (map read) . words

checksum :: [Int] -> Int
checksum list = (maximum list) - (minimum list)

-- PART B

cartesianProduct :: [Int] -> [(Int, Int)]
cartesianProduct list = [
    (i, j) |
        i <- list,
        j <- list
    ]

-- Takes a function that expects two arguments, turns it into a function that takes a pair
onPair :: (a -> a -> b) -> ((a, a) -> b)
onPair = uncurry

divisible x y = mod x y == 0

checksum' :: [Int] -> Int
checksum' =
      onPair quot
    . head -- Should only be one result anyway
    . filter (onPair divisible)
    . filter (onPair (/=))
    . cartesianProduct
