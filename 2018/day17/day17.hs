{-# LANGUAGE NamedFieldPuns #-}
import Data.Char (isNumber)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S, munch1, string, get, pfail, endBy, eof)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

main = do
    input <- getContents
    let veins = runParser inputParser input
        tiles = buildGrid veins
        (_, _, minY, maxY) = gridRange tiles
        result = fall maxY tiles (500, minY)

    putStrLn $ showGrid result
    print $ length $ filter (/= Clay) $  map snd $ Map.toList $ result

-- Filling logic

isEmpty :: TileMap -> Coord -> Bool
isEmpty grid coord =
    case Map.lookup coord grid of
        Nothing -> True
        Just FlowingWater -> True
        _ -> False

fall :: Int -> TileMap -> Coord -> TileMap
fall gridMaxY grid coord =
    if
        (snd coord) > gridMaxY || (not $ isEmpty grid coord)
    then
        grid
    else let
        spaceBelow = (down coord)
        grid2 = if isEmpty grid spaceBelow then fall gridMaxY grid spaceBelow else grid
    in if isEmpty grid2 spaceBelow
        then setGrid grid2 coord FlowingWater
        -- Do flood or spill
        else let
            holes = findHoles grid2 coord
            fillHole _grid holeCoord = fall gridMaxY _grid holeCoord
            grid3 = foldl fillHole grid2 holes
            shouldFill = not $ any (isEmpty grid3) holes
        in if shouldFill then
            fill grid3 coord
        else
            flow grid3 coord


findHoles :: TileMap -> Coord -> [Coord]
findHoles grid coord =
    catMaybes [
        scanDir grid left coord,
        scanDir grid right coord
    ]

scanDir :: TileMap -> (Coord -> Coord) -> Coord -> Maybe Coord
scanDir grid dir coord =
    if isEmpty grid (down coord) then
        Just (down coord)
    else if isEmpty grid (dir coord) then
        scanDir grid dir (dir coord)
    else Nothing

fill :: TileMap -> Coord -> TileMap
fill grid coord =
    fillDir right (fillDir left grid (left coord)) coord

fillDir :: (Coord -> Coord) -> TileMap -> Coord -> TileMap
fillDir dir grid coord =
    if isEmpty grid coord then
        fillDir dir (setGrid grid coord SettledWater) (dir coord)
    else grid

flow :: TileMap -> Coord -> TileMap
flow grid coord =
    flowDir left (flowDir right grid coord) coord

flowDir :: (Coord -> Coord) -> TileMap -> Coord -> TileMap
flowDir dir grid coord =
    if not $ isEmpty grid coord then
        grid
    else let
        newGrid = (setGrid grid coord FlowingWater)
    in if isEmpty grid (down coord) then
        newGrid
    else
        flowDir dir newGrid (dir coord)


-- TileMap

-- gg :: TileMap
-- gg = Map.fromList [((495,2),Clay),((495,3),Clay),((495,4),Clay),((495,5),Clay),((495,6),Clay),((495,7),Clay),((496,7),Clay),((497,7),Clay),((498,2),Clay),((498,3),Clay),((498,4),Clay),((498,7),Clay),((498,10),Clay),((498,11),Clay),((498,12),Clay),((498,13),Clay),((499,7),Clay),((499,13),Clay),((500,7),Clay),((500,13),Clay),((501,3),Clay),((501,4),Clay),((501,5),Clay),((501,6),Clay),((501,7),Clay),((501,13),Clay),((502,13),Clay),((503,13),Clay),((504,10),Clay),((504,11),Clay),((504,12),Clay),((504,13),Clay),((506,1),Clay),((506,2),Clay)]


data Tile
    = Clay
    | FlowingWater
    | SettledWater
    deriving (Show, Eq)

type TileMap = Map.Map Coord Tile

setGrid :: TileMap -> Coord -> Tile -> TileMap
setGrid grid coord tile = Map.insert coord tile grid

buildGrid :: [Vein] -> TileMap
buildGrid = Map.fromList . (>>= asTiles)

showTile :: TileMap -> Coord -> Char
showTile grid coord =
    case Map.lookup coord grid of
        Just Clay -> '#'
        Just FlowingWater -> '|'
        Just SettledWater -> '~'
        Nothing -> '.'

showGrid :: TileMap -> String
showGrid grid =
    let
        (minX, maxX, minY, maxY) = gridRange $ grid
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
