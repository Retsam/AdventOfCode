main = do
    str <- getContents
    putStrLn (show . (map ((minus 1) . read . (!! 1) . words)) . lines $ str)
