{-# LANGUAGE NamedFieldPuns #-}
import Data.Char (isNumber)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, munch1, string, get, pfail, endBy, eof)
import qualified Data.Map as Map

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
    input <- getContents
    let veins = runParser inputParser input
        tiles = buildGrid veins
        (_, _, minY, maxY) = gridRange tiles
        fallFromOrigin = flip fall $ (500, minY)
        result = fallFromOrigin Grid { --converge fallFromOrigin Grid {
            tileMap = tiles,
            gridMaxY = maxY
        }

    putStrLn $ showGrid result
    print $ length $ filter (/= Clay) $  map snd $ Map.toList $ tileMap result

-- Filling logic
isEmpty :: Grid -> Coord -> Bool
isEmpty grid coord =
    case Map.lookup coord (tileMap grid) of
        Nothing -> True
        Just FlowingWater -> True
        _ -> False

fallFromOrigin = flip fall $ (500, 0)

fall :: Grid -> Coord -> Grid
fall grid coord =
    if (snd coord) > gridMaxY grid then
        grid
    else case isEmpty grid (down coord) of
        True -> fall (setGrid grid coord FlowingWater) (down coord)
        False -> spread grid coord

data ScanResult
    = Wall
    | DropAt Coord


spread :: Grid -> Coord -> Grid
spread grid coord =
    let
        scanL = scanDir grid left coord
        scanR = scanDir grid right coord
    in case (scanL, scanR) of
        (Wall, Wall) -> fill grid coord
        _ -> flow grid coord

scanDir :: Grid -> (Coord -> Coord) -> Coord -> ScanResult
scanDir grid dir coord =
    if isEmpty grid (down coord) then
        DropAt coord
    else if isEmpty grid (dir coord) then
        scanDir grid dir (dir coord)
    else Wall

fill :: Grid -> Coord -> Grid
fill grid coord =
    fillDir right (fillDir left grid (left coord)) coord

fillDir :: (Coord -> Coord) -> Grid -> Coord -> Grid
fillDir dir grid coord =
    if isEmpty grid coord then
        fillDir dir (setGrid grid coord SettledWater) (dir coord)
    else grid

flow :: Grid -> Coord -> Grid
flow grid coord =
    flowDir left (flowDir right grid coord) coord

flowDir :: (Coord -> Coord) -> Grid -> Coord -> Grid
flowDir dir grid coord =
    let
        newGrid = (setGrid grid coord FlowingWater)
    in if isEmpty grid (down coord) then
        fall grid coord
    else if isEmpty grid (dir coord) then
        flowDir dir newGrid (dir coord)
    else newGrid -- full


-- Grid

gg :: Grid
gg = Grid {
    tileMap = Map.fromList [((495,2),Clay),((495,3),Clay),((495,4),Clay),((495,5),Clay),((495,6),Clay),((495,7),Clay),((496,7),Clay),((497,7),Clay),((498,2),Clay),((498,3),Clay),((498,4),Clay),((498,7),Clay),((498,10),Clay),((498,11),Clay),((498,12),Clay),((498,13),Clay),((499,7),Clay),((499,13),Clay),((500,7),Clay),((500,13),Clay),((501,3),Clay),((501,4),Clay),((501,5),Clay),((501,6),Clay),((501,7),Clay),((501,13),Clay),((502,13),Clay),((503,13),Clay),((504,10),Clay),((504,11),Clay),((504,12),Clay),((504,13),Clay),((506,1),Clay),((506,2),Clay)],
    gridMaxY = 13
}

data Tile
    = Clay
    | FlowingWater
    | SettledWater
    deriving (Show, Eq)

type TileMap = Map.Map Coord Tile
data Grid = Grid {
  tileMap :: TileMap,
  gridMaxY :: Int
} deriving (Show, Eq)

setGrid :: Grid -> Coord -> Tile -> Grid
setGrid grid coord tile = grid { tileMap = Map.insert coord tile (tileMap grid) }

buildGrid :: [Vein] -> TileMap
buildGrid = Map.fromList . (>>= asTiles)

showTile :: Grid -> Coord -> Char
showTile grid coord =
    case Map.lookup coord (tileMap grid) of
        Just Clay -> '#'
        Just FlowingWater -> '|'
        Just SettledWater -> '~'
        Nothing -> '.'

showGrid :: Grid -> String
showGrid grid =
    let
        (minX, maxX, minY, maxY) = gridRange $ tileMap grid
        showRow y = map (\x -> showTile grid (x, y)) [minX-1..maxX+1]
    in concat (map ((++"\n") . showRow) [minY-1..maxY+1])

gridRange :: TileMap -> (Int, Int, Int, Int)
gridRange grid =
    let
        coords = map fst $ filter ((== Clay) . snd) $ Map.toList $ grid
        xs = map fst coords
        ys = map snd coords
    in (minimum xs, maximum xs, minimum ys, maximum ys)

-- Vein logic

data Vein
    = Row {y :: Int, minX :: Int, maxX :: Int}
    | Col {x :: Int, minY :: Int, maxY :: Int}
    deriving (Show)

asTiles :: Vein -> [(Coord, Tile)]
asTiles = map (\c -> (c, Clay)) . _asTiles

_asTiles :: Vein -> [Coord]
_asTiles Row { y, minX, maxX } =
    map (\x -> (x, y)) [minX..maxX]
_asTiles Col { x, minY, maxY } =
    map (\y -> (x, y)) [minY..maxY]

-- Coord

type Coord = (Int, Int)

down :: Coord -> Coord
down (x, y) = (x, y + 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

-- input parsing
inputParser :: ReadP [Vein]
inputParser = do
    res <- endBy veinParser (string "\n")
    eof
    return res

veinParser :: ReadP Vein
veinParser = do
    xOrY <- get
    string "="
    val <- intParser
    string ", "
    get -- yOrX
    string "="
    minYOrX <- intParser
    string ".."
    maxYOrX <- intParser
    case xOrY of
        'x' -> return Col { x = val, minY=minYOrX, maxY=maxYOrX}
        'y' -> return Row { y = val, minX=minYOrX, maxX=maxYOrX}
        _ -> pfail

-- parser utils

runParser :: Show a => ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, remainder)] -> error ("Didn't consume whole input" ++ remainder)
        r -> error ("Input parse error" ++ (show r))

intParser :: ReadP Int
intParser = fmap read (munch1 isNumber)
