main = do
  input <-
    getContents
  let points = parseInput input
  print $ cheapestBy standardCost points
  print $ cheapestBy triangleCost points

parseInput :: String -> [Int]
parseInput input = read $ "[" ++ input ++ "]"

standardCost :: Int -> Int -> Int
standardCost x y = abs $ y - x

triangleCost :: Int -> Int -> Int
triangleCost x y = n * (n + 1) `div` 2
  where
    n = abs $ y - x

cheapestBy :: (Int -> Int -> Int) -> [Int] -> Int
cheapestBy cost points = minimum $ map costFromPoint range
  where
    range = [minimum points .. maximum points]
    costFromPoint x = sum (map (cost x) points)
