import qualified Data.Set as Set

data WorkingState
    = WorkingState {
        current :: Int,
        next :: [Int],
        prev :: Set.Set Int
    }

main = interact (show . solve . (map parseNumber) . lines)

-- Supports a leading '+' character
parseNumber :: String -> Int
parseNumber ('+':xs) = read xs
parseNumber xs = read xs

solve :: [Int] -> Int
solve input = loop WorkingState {
    current = 0,
    next = cycle input,
    prev = Set.singleton 0
}

loop :: WorkingState -> Int
loop WorkingState {current = c, next = (n: ns), prev = p} =
    let newSum = c + n
        newSet = Set.insert newSum p
    in case Set.member newSum p of
        True -> newSum
        False -> loop WorkingState { current = newSum, next = ns, prev = Set.insert newSum p}

