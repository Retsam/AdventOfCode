import Data.Either
import Data.List (sort)
import Debug.Trace

main = do
  input <-
    getContents
  let stuff = lines input
  print $ part1 stuff
  print $ part2 stuff

part1 :: [String] -> Int
part1 = sum . lefts . map (lineScore [])

part2 :: [String] -> Int
part2 = middle . sort . map (autocompleteScore . reverse) . rights . map (lineScore [])

middle list = list !! (length list `div` 2)

safeHead :: [a] -> Maybe a
safeHead list = if null list then Nothing else Just $ head list

lineScore :: String -> String -> Either Int String
lineScore expected "" = Right expected
lineScore expected (c : cs) = case c of
  '<' -> expect '>'
  '[' -> expect ']'
  '(' -> expect ')'
  '{' -> expect '}'
  '>' -> match '>' 25137
  ']' -> match ']' 57
  ')' -> match ')' 3
  '}' -> match '}' 1197
  other -> error ("Unexpected " ++ [other])
  where
    expect newChar = lineScore (newChar : expected) cs
    match expectedChar errorScore = case safeHead expected of
      Just foundChar ->
        if foundChar == expectedChar
          then lineScore (tail expected) cs
          else Left errorScore
      _ -> Left errorScore

autocompleteScore "" = 0
autocompleteScore (h : rest) = letterScore + (5 * autocompleteScore rest)
  where
    letterScore = case h of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      _ -> error "Unexpected"
