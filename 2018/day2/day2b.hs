main = interact (
    show
    . (filter (\(a, b) -> difference a b == 1))
    . cartProd
    . lines )

cartProd :: [a] -> [(a, a)]
cartProd xs = [(x,y) | x <- xs, y <- xs]

difference :: String -> String -> Int
difference a b =
    length $ filter (\(a, b) -> a /= b) $ zip a b

