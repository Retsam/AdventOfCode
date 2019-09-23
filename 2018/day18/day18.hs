import Data.Maybe (catMaybes)

type Tile = Char
type State = [[Tile]]

tree = '|'
open = '.'
yard = '#'

-- Part 1
main = interact (
    lines |> iterate step |> (!! 10) |> score |> show |> (++"\n")
    )


score :: State -> Int
score state =
    count tree tiles
    * count yard tiles
    where tiles = unlines state

step :: State -> State
step state =
    map (map (uncurry $ _step state))
    $ toPairs state

_step :: State -> (Int, Int) -> Tile -> Tile
_step state coord = nextTile (getNeighbors state coord)

nextTile :: [Tile] -> Tile -> Tile
nextTile neighbors '.' =
    if (count tree neighbors) >= 3 then tree else open
nextTile neighbors '|' =
    if (count yard neighbors) >= 3 then yard else tree
nextTile neighbors '#' =
    if
        ((count yard neighbors) >= 1) && ((count tree neighbors) >= 1)
    then yard else open

nextTile _ t = error ("Bad tile: " ++ [t])

-- Mapping helpers
toPairs :: State -> [[((Int, Int), Tile)]]
toPairs =
    map (\(row, rowTiles) ->
        map (\(col, tile) ->
            ((row, col), tile)
        ) $ zip [0..] rowTiles
    ) . zip [0..]

-- fromPairs :: [[((Int, Int), Tile)]] -> State
-- fromPairs = map (map (\(_, t) -> t))


count :: Tile -> [Tile] -> Int
count tile = length . filter (==tile)

-- State logic

getNeighbors :: State -> (Int, Int) -> [Tile]
getNeighbors state (row, col)
    = catMaybes $ map (getTile state) [
        (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
        (row    , col - 1),                 (row    , col + 1),
        (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
    ]

getTile :: State -> (Int, Int) -> Maybe Tile
getTile state (row, col) =
    state !? row >>= (!? col)

parseInput :: String -> State
parseInput = lines


-- Util
a |> b = b . a
a .> b = b a

(!?) :: [a] -> Int -> Maybe a
a !? b = if b >= 0 && b < (length a) then Just (a !! b) else Nothing
