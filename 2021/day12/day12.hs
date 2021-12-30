import Control.Monad.Writer
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.List (intercalate, nub)
import Text.Parsec

main = do
  input <- getContents
  let parsed = parse parseInput "input" input
  let paths = fromRight (error "Parse") parsed >>= pathAndReverse

  print $ part1 paths
  print $ part2 paths

pathAndReverse (a, b)
  | a == "start" || b == "end" = [(a, b)]
  | a == "end" || b == "start" = [(b, a)]
  | otherwise = [(a, b), (b, a)]

part1 paths = length $ findPath paths part1CanVisit [] "start"

part2 paths = length $ findPath paths part2CanVisit [] "start"

isSmallCave loc = loc == map toLower loc

part1CanVisit prev loc = not (isSmallCave loc && loc `elem` prev)

hasRepeatSmallCave prev = length smallCaves /= length (nub smallCaves)
  where
    smallCaves = filter isSmallCave prev

part2CanVisit prev loc = not (hasRepeatSmallCave prev) || part1CanVisit prev loc

findPath :: [(String, String)] -> ([String] -> String -> Bool) -> [String] -> String -> [String]
findPath _ _ prev "end" = [intercalate "," prev]
findPath paths canVisit prev loc = mconcat $ map (findPath paths canVisit (loc : prev)) possibleMoves
  where
    possibleMoves = filter (canVisit (loc : prev)) $ map snd (filter ((loc ==) . fst) paths)

parseLoc :: Parsec String st String
parseLoc = many (noneOf "-\n")

parseLine = do
  [from, to] <- parseLoc `sepBy` char '-'
  return (from, to)

parseInput = endBy parseLine (string "\n")
