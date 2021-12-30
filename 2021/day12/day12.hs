import Data.Char (toLower)
import Data.Either (fromRight)
import Text.Parsec

main = do
  input <- getContents
  let parsed = parse parseInput "input" input
  let paths = fromRight (error "Parse") parsed >>= pathAndReverse
  print $ part1 paths

pathAndReverse ("start", b) = [("start", b)]
pathAndReverse (a, "end") = [(a, "end")]
pathAndReverse (a, b) = [(a, b), (b, a)]

isSmallCave loc = loc == map toLower loc

part1CanVisit prev loc = not (isSmallCave loc && loc `elem` prev)

part1 paths = findPath paths part1CanVisit [] "start"

findPath :: [(String, String)] -> ([String] -> String -> Bool) -> [String] -> String -> Int
findPath _ _ _ "end" = 1
findPath paths canVisit prev loc = sum $ map (findPath paths canVisit (loc : prev)) possibleMoves
  where
    possibleMoves = filter (canVisit prev) $ map snd (filter ((loc ==) . fst) paths)

parseLoc :: Parsec String st String
parseLoc = many (noneOf "-\n")

parseLine = do
  [from, to] <- parseLoc `sepBy` char '-'
  return (from, to)

parseInput = endBy parseLine (string "\n")
