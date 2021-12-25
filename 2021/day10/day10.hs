import Debug.Trace

main = do
  input <-
    getContents
  let stuff = lines input
  print $ part1 stuff

part1 :: [String] -> Int
part1 = sum . map (lineScore [])

safeHead :: [a] -> Maybe a
safeHead list = if null list then Nothing else Just $ head list

lineScore :: String -> String -> Int
lineScore _ "" = 0
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
          else errorScore
      _ -> errorScore
