main :: IO ()
main = interact ((++ "\n") . show . part1)

parseInput :: String -> [Int]
parseInput = map read . lines

-- Converts [A, B, C, D, E]
--    to [(A, B), (B, C), (C, D), (D, E)] (offset 1)
--    or [(A, D), (B, E)] (offset 3)
offsetZip :: Int -> [b] -> [(b, b)]
offsetZip offset arr = zip arr (drop offset arr)

part1 :: String -> Int
part1 = length . filter (uncurry (<)) . offsetZip 1 . parseInput

part2 :: String -> Int
part2 = length . filter (uncurry (<)) . offsetZip 3  . parseInput
