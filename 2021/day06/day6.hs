main = do
  input <-
    getContents
  let fish = buildCounts $ parseInput input
  let generations = iterate step fish
  print $ sum (generations !! 80)
  print $ sum (generations !! 256)

parseInput :: String -> [Int]
parseInput input = read $ "[" ++ input ++ "]"

buildCounts :: [Int] -> [Int]
buildCounts input = map (\x -> length $ filter (== x) input) [0 .. 8]

step :: [Int] -> [Int]
step [x0, x1, x2, x3, x4, x5, x6, x7, x8] = [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]
step _ = error "wrong list"
