import Data.Char (isNumber, isSpace)
import Data.List (find, transpose)
import Data.Set (Set, member)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP (ReadP, char, eof, munch, munch1, readP_to_S, sepBy, sepBy1, skipSpaces, string)

type Board = [[Int]]

main = do
  input <- getContents
  let (nums, boards) = runParser parseInput input
  let initState = (Set.empty, nums, boards)
  print $ part1 initState
  print $ part2 initState

-- part 1
wins :: Set Int -> Board -> Bool
wins drawn board = any lineWins board || any lineWins (transpose board)
  where
    lineWins = all (`member` drawn)

type State = (Set Int, [Int], [Board])

part1 :: State -> Int
part1 (drawn, toCall : rest, boards) =
  case find (wins newDrawn) boards of
    Just board -> toCall * score newDrawn board
    Nothing -> part1 (newDrawn, rest, boards)
  where
    newDrawn = Set.insert toCall drawn
part1 (_, [], _) = error "Ran out of numbers"

score :: Set Int -> [[Int]] -> Int
score drawn = sum . filter (not . (`member` drawn)) . concat

--- part 2

part2 :: State -> Int
part2 (drawn, called, [board]) = part1 (drawn, called, [board])
part2 (drawn, toCall : rest, boards) =
  part2 (newDrawn, rest, filter (not . wins newDrawn) boards)
  where
    newDrawn = Set.insert toCall drawn
part2 (_, [], _) = error "Ran out of numbers"

-- parsing

parseNumbers :: ReadP [Int]
parseNumbers = sepBy1 parseInt (char ',')

parseInt :: ReadP Int
parseInt = fmap read (munch1 isNumber)

parseBingoCard :: ReadP Board
parseBingoCard = parseBingoCardLine `sepBy1` char '\n'

parseBingoCardLine :: ReadP [Int]
parseBingoCardLine = munch (== ' ') >> parseInt `sepBy1` munch1 (== ' ')

parseInput :: ReadP ([Int], [Board])
parseInput = do
  nums <- parseNumbers
  skipSpaces
  cards <- parseBingoCard `sepBy` string "\n\n"
  skipSpaces
  eof
  return (nums, cards)

runParser :: ReadP a -> String -> a
runParser parser input =
  case readP_to_S parser input of
    -- Should be exactly one result, which consumes the entire input
    [(xs, "")] -> xs
    [] -> error "Input parse error"
    _ -> error "Didn't consume whole input"
