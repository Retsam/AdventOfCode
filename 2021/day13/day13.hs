import Data.Either (fromRight)
import Data.List (intercalate, nub)
import Text.Parsec

main = do
  input <- getContents
  let parsed = parse parseInput "input" input
  let stuff = fromRight (error "oops") parsed
  print $ part1 stuff
  putStrLn $ part2 stuff

part1 (points, fold : _) = length $ nub $ doFold points fold
part1 _ = error "no folds"

part2 (points, folds) = display $ foldl doFold points folds

display :: [(Int, Int)] -> [Char]
display points = intercalate "\n" output
  where
    maxX = maximum $ map fst points
    maxY = maximum $ map snd points
    char pt
      | pt `elem` points = '#'
      | otherwise = '.'
    output = [[char (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]

doFold points (dir, val) = map updatePoint points
  where
    updatePoint (x, y)
      | dir == 'x' && x > val = (val - abs (x - val), y)
      | dir == 'y' && y > val = (x, val - abs (y - val))
      | otherwise = (x, y)

parseLines = (`endBy` string "\n")

parseInput = do
  points <- parseLines (try parsePoint)
  folds <- string "\n" *> parseLines parseFold
  return (points, folds)

parseFold = do
  dir <- string "fold along " *> oneOf "xy"
  val <- char '=' *> parseInt
  return (dir, val)

parsePoint = do
  a <- parseInt
  b <- string "," *> parseInt
  return (a, b)

parseInt :: Parsec String st Int
parseInt = read <$> many1 digit
