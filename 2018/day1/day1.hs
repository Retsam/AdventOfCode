main = interact (show . sum . (map parseNumber) . lines)

parseNumber :: String -> Int
parseNumber ('+':xs) = read xs
parseNumber xs = read xs
