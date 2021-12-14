import Control.Monad (liftM2)
import Data.Char (isAlpha)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Text.Internal.Read (IParser (runP))
import Text.ParserCombinators.ReadP (ReadP, eof, munch1, readP_to_S, sepBy1, string)

main = do
  input <-
    getContents
  let cases = map (runParser parseLine) (lines input)
  print $ part1 cases
  print $ part2 cases

part1 :: [([String], [String])] -> Int
part1 = sum . map (length . filter isKnownLength . snd)
  where
    isKnownLength x = length x `elem` [2, 3, 4, 7]

part2 :: [([String], [String])] -> Int
part2 = sum . map solve

contains char pattern = isJust $ find (== char) pattern

assertFind :: (c -> Bool) -> [c] -> c
assertFind x = fromJust . find x

solve :: ([String], [String]) -> Int
solve (inputs, outputs) = read $ map (decode (getHints inputs)) outputs

data Hints = Hints
  { c :: Char,
    d :: Char,
    e :: Char
  }
  deriving (Show)

getHints :: [String] -> Hints
getHints patterns = Hints {c = c, d = d, e = e}
  where
    findBySize n = assertFind ((== n) . length) patterns
    countUsageInDigits n c = (== n) . length $ filter (contains c) patterns

    onePattern = findBySize 2
    c = assertFind (countUsageInDigits 8) onePattern
    fourPattern = findBySize 4
    d = assertFind (countUsageInDigits 7) fourPattern
    e = assertFind (countUsageInDigits 4) "abcdefg"

decode :: Hints -> String -> Char
decode hints pattern =
  case length pattern of
    2 -> '1'
    3 -> '7'
    4 -> '4'
    5 -> if contains (e hints) pattern then '2' else if contains (c hints) pattern then '3' else '5'
    6 -> if not $ contains (d hints) pattern then '0' else if contains (e hints) pattern then '6' else '9'
    7 -> '8'
    _ -> error "idk"

parseInput = parseLine `sepBy1` string "\n" <* eof

parsePatterns :: ReadP [String]
parsePatterns = munch1 isAlpha `sepBy1` string " "

parseLine :: ReadP ([String], [String])
parseLine = parseTuple parsePatterns (string " | ") <* eof

parseTuple :: ReadP item -> ReadP sep -> ReadP (item, item)
parseTuple body sep = (,) <$> body <*> (sep *> body)

runParser :: ReadP a -> String -> a
runParser parser input =
  case readP_to_S parser input of
    -- Should be exactly one result, which consumes the entire input
    [(xs, "")] -> xs
    [] -> error "Input parse error"
    _ -> error "Didn't consume whole input"
