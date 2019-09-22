{-# LANGUAGE NamedFieldPuns #-}
import Debug.Trace
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
    -- Part 1
    print $ length $ filter (/= Clay) $  map snd $ Map.toList $ result
    -- Part 2
    print $ length $ filter (== SettledWater) $  map snd $ Map.toList $ result

-- Filling logic

isEmpty :: TileMap -> Coord -> Bool
isEmpty grid coord =
    case Map.lookup coord grid of
        Nothing -> True
        Just FlowingWater -> True
        _ -> False

type MaxY = Int

-- Recursive algorithm for falling water; when it hits something, flows
fall :: MaxY -> TileMap -> Coord -> TileMap
fall gridMaxY grid coord =
    if
        (snd coord) > gridMaxY ||
        (not $ Map.lookup coord grid == Nothing)
    then
        grid
    else let
        spaceBelow = down coord
        newGrid = if isEmpty grid spaceBelow then fall gridMaxY grid spaceBelow else grid
    in if isEmpty newGrid spaceBelow
        then setGrid newGrid coord FlowingWater
        else flow gridMaxY newGrid coord

-- First flows in both directions, attempting to fill holes,
-- until it hits a wall or fails to fill a hole.  Then fills.
flow :: MaxY -> TileMap -> Coord -> TileMap
flow gridMaxY grid coord =
    let
        (grid2, hitLeftWall) = flowDir gridMaxY grid left coord
        (grid3, hitRightWall) = flowDir gridMaxY grid2 right coord
        fillTile = if hitLeftWall && hitRightWall then SettledWater else FlowingWater
    in fill fillTile grid3 coord

flowDir :: MaxY -> TileMap -> (Coord -> Coord) -> Coord -> (TileMap, Bool)
flowDir gridMaxY grid dir coord =
    let
        spaceBelow = (down coord)
        nextSpace = (dir coord)
        -- if the space below isn't empty, noops
        gridAfterFall = fall gridMaxY grid spaceBelow
        filledBelow = (not $ isEmpty gridAfterFall spaceBelow)
        isBlocked = (not $ isEmpty gridAfterFall nextSpace)
    in if filledBelow && (not isBlocked) then
        flowDir gridMaxY gridAfterFall dir nextSpace
    else
        -- if it's filled below, but we're not flowing we must have hit a wall
        (gridAfterFall, filledBelow)


-- Fill with either flowing water (if bounded on both sides)
-- otherwise fills with flowing water.
fill :: Tile -> TileMap -> Coord -> TileMap
fill tile grid coord =
    fillDir tile right (fillDir tile left grid coord) coord

fillDir :: Tile -> (Coord -> Coord) -> TileMap -> Coord -> TileMap
fillDir fillTile dir grid coord =
    let
        spaceBelow = down coord
        nextSpace = dir coord
        filledBelow = (not $ isEmpty grid spaceBelow)
        isBlocked = (not $ isEmpty grid nextSpace)
        filledGrid = setGrid grid coord fillTile
    in if filledBelow && (not isBlocked) then
        fillDir fillTile dir filledGrid (dir coord)
    else filledGrid

-- TileMap

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
