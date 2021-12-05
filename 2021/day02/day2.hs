main = do
  input <- getContents
  let commands = map parseCommand $ lines input
  let part1 = uncurry (*) $ foldl updateState (0, 0) commands
  let part2 = let (x, d, _) = foldl updateStateWithAim (0, 0, 0) commands in (x * d)
  print (part1, part2)

type Cmd = (String, Int)

parseCommand :: String -> Cmd
parseCommand input = let [a, b] = words input in (a, read b)

type Pos = (Int, Int) -- Horiz, Depth

updateState :: Pos -> Cmd -> Pos
updateState (x, depth) cmd = case cmd of
  ("forward", dist) -> (x + dist, depth)
  ("up", y) -> (x, depth - y)
  ("down", y) -> (x, depth + y)
  _ -> error "Bad input"

type PosWithAim = (Int, Int, Int) -- Horiz, Depth, Aim

updateStateWithAim :: PosWithAim -> Cmd -> PosWithAim
updateStateWithAim (x, depth, aim) cmd = case cmd of
  ("forward", dist) -> (x + dist, depth + (aim * dist), aim)
  ("up", y) -> (x, depth, aim - y)
  ("down", y) -> (x, depth, aim + y)
  _ -> error "Bad input"
