import qualified Data.Map as Map
import Data.Maybe

type X = Int
type Y = Int
type Erosion = Int
type GeoIndex = Int
data RegionType = Rocky | Narrow | Wet deriving Show

-- Example
-- input = [10, 10, 510]
-- Problem
input = [9, 796, 6969]

targetX = input !! 0
targetY = input !! 1
depth = input !! 2

type ErosionMap = Map.Map (X, Y) Erosion

main = print $ risk_level erosion_map

empty_map :: ErosionMap
empty_map = Map.empty

erosion_map =
    foldl update_map_at empty_map (cartProd [0..targetX] [0..targetY])

update_map_at :: ErosionMap -> (X, Y) -> ErosionMap
update_map_at map (x, y) = Map.insert (x, y) (erosion_level x y map) map

erosion_level :: X -> Y -> ErosionMap -> Erosion
erosion_level x y = erosion_from_geo_index . (geo_index x y)

geo_index :: X -> Y -> ErosionMap -> GeoIndex
geo_index 0 0 _ = 0
geo_index x 0 _ = x * 16807
geo_index 0 y _ = y * 48271
geo_index x y map =
    if (x == targetX) && (y == targetY)
        then 0
        else (map Map.! (x - 1, y)) * map Map.! ((x, y - 1))

erosion_from_geo_index :: GeoIndex -> Erosion
erosion_from_geo_index idx = (idx + depth) `mod` 20183


region_type :: Erosion -> RegionType
region_type e =
    case e `mod` 3 of
        0 -> Rocky
        1 -> Wet
        2 -> Narrow

region_icon :: RegionType -> Char
region_icon Rocky = '.'
region_icon Wet = '='
region_icon Narrow = '|'

risk_level :: ErosionMap -> Int
risk_level = Map.foldl (+) 0 . Map.map (`mod` 3)

-- Utils
cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


showGrid :: Map.Map (Int, Int) Char -> String

showGrid grid =
    let
        (minX, maxX, minY, maxY) = gridRange $ grid
        showChar y x = case Map.lookup (x, y) grid of
            Just c -> c
            Nothing -> ' '
        showRow y = map (showChar y) [minX..maxX]
    in concat (map ((++"\n") . showRow) [minY..maxY])

gridRange :: Map.Map (Int, Int) Char -> (Int, Int, Int, Int)
gridRange grid =
    let
        coords = map fst $ Map.toList $ grid
        xs = map fst coords
        ys = map snd coords
    in (minimum xs, maximum xs, minimum ys, maximum ys)
