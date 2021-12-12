import Data.Char (isNumber, isSpace)
import Data.List (group, sort)
import Data.Text.Internal.Read (IParser (runP))
import Text.ParserCombinators.ReadP (ReadP, eof, munch1, readP_to_S, sepBy1, string)
import qualified Text.ParserCombinators.ReadP as ReadP

type Point = (Int, Int)

main = do
  input <- getContents
  let pairs = map (runParser parseLine) (lines input)
  print $ part1 pairs
  print $ part2 pairs

part1 :: [(Point, Point)] -> Int
part1 = countIntersections . filter isStraightLine

part2 :: [(Point, Point)] -> Int
part2 = countIntersections

countIntersections :: [(Point, Point)] -> Int
countIntersections = length . filter (> 1) . map length . group . sort . (>>= pointsBetween)

range start end step = [start, start + step .. end]

pointsBetween :: (Point, Point) -> [Point]
pointsBetween ((x1, y1), (x2, y2)) = zip (range x1 x2 dx) (range y1 y2 dy)
  where
    dx = signum (x2 - x1)
    dy = signum (y2 - y1)

isStraightLine :: (Point, Point) -> Bool
isStraightLine ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

-- Parsing

parseInt :: ReadP Int
parseInt = fmap read (munch1 isNumber)

parseTuple :: ReadP item -> ReadP sep -> ReadP (item, item)
parseTuple body sep = (,) <$> body <*> (sep *> body)

parseLine :: ReadP (Point, Point)
parseLine = parseTuple (parseTuple parseInt $ string ",") (string " -> ") <* eof

runParser :: ReadP a -> String -> a
runParser parser input =
  case readP_to_S parser input of
    -- Should be exactly one result, which consumes the entire input
    [(xs, "")] -> xs
    [] -> error "Input parse error"
    _ -> error "Didn't consume whole input"
